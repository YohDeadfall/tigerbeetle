const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

const constants = @import("../constants.zig");
const stdx = @import("../stdx.zig");

const build_options = @import("vsr_options");

const vsr = @import("vsr");
const IO = vsr.io.IO;
const Storage = vsr.storage.Storage;
const StateMachine = vsr.state_machine.StateMachineType(Storage, constants.state_machine_config);
const MessagePool = vsr.message_pool.MessagePool;

const tb = vsr.tigerbeetle;

fn print(comptime fmt: []const u8, args: anytype) !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.print(fmt, args);
}

pub const Parser = struct {
    input: []const u8,
    offset: usize = 0,

    pub const Error = error{
        BadKeyValuePair,
        BadValue,
        BadOperation,
        BadIdentifier,
        MissingEqualBetweenKeyValuePair,
        NoSyntaxMatch,
    };

    pub const Operation = enum {
        none,
        help,
        create_accounts,
        create_transfers,
        lookup_accounts,
        lookup_transfers,
    };

    pub const LookupSyntaxTree = struct {
        id: u128,
    };

    pub const ObjectSyntaxTree = union(enum) {
        account: tb.Account,
        transfer: tb.Transfer,
        id: LookupSyntaxTree,
    };

    pub const StatementSyntaxTree = struct {
        operation: Operation,
        // Must be an array of pointers to structs because
        // there is a Zig bug that causes segfaults sometimes
        // when expanding capacity for an array of structs.
        // https://github.com/ziglang/zig/issues/8655
        args: []*ObjectSyntaxTree,
    };

    fn fail_at(
        parse: *Parser,
        comptime fmt: []const u8,
        args: anytype,
    ) !void {
        const target = target: {
            var position_cursor: usize = 0;
            var position_line: usize = 1;
            var lines = std.mem.split(u8, parse.input, "\n");
            while (lines.next()) |line| {
                if (position_cursor + line.len >= parse.offset) {
                    break :target .{
                        .line = line,
                        .position_line = position_line,
                        .position_column = parse.offset - position_cursor,
                    };
                } else {
                    position_line += 1;
                    position_cursor += line.len + 1; // +1 for trailing newline.
                }
            } else unreachable;
        };

        const stderr = std.io.getStdErr().writer();
        try stderr.print("Fail near line {}, column {}:\n\n{s}\n", .{
            target.position_line,
            target.position_column,
            target.line,
        });
        var column = target.position_column;
        while (column > 0) {
            try stderr.print(" ", .{});
            column -= 1;
        }
        try stderr.print("^ Near here.\n\n", .{});
        try stderr.print(fmt, args);
    }

    fn eat_whitespace(parse: *Parser) void {
        while (parse.offset < parse.input.len and
            std.ascii.isSpace(parse.input[parse.offset]))
        {
            parse.offset += 1;
        }
    }

    fn parse_identifier(parse: *Parser) []const u8 {
        parse.eat_whitespace();
        const after_whitespace = parse.offset;

        while (parse.offset < parse.input.len and
            (std.ascii.isAlpha(parse.input[parse.offset]) or
            parse.input[parse.offset] == '_'))
        {
            parse.offset += 1;
        }

        return parse.input[after_whitespace..parse.offset];
    }

    fn parse_syntax_char(parse: *Parser, syntax_char: u8) !void {
        parse.eat_whitespace();

        if (parse.offset < parse.input.len and
            parse.input[parse.offset] == syntax_char)
        {
            parse.offset += 1;
            return;
        }

        return Error.NoSyntaxMatch;
    }

    fn parse_value(
        parse: *Parser,
    ) []const u8 {
        parse.eat_whitespace();
        const after_whitespace = parse.offset;

        while (parse.offset < parse.input.len) {
            const c = parse.input[parse.offset];
            if (!(std.ascii.isAlNum(c) or c == '_' or c == '|')) {
                // Allows flag fields to have whitespace before a '|'.
                var copy = Parser{ .input = parse.input, .offset = parse.offset };
                copy.eat_whitespace();
                if (copy.offset < parse.input.len and parse.input[copy.offset] == '|') {
                    parse.offset = copy.offset;
                    continue;
                }

                // Allow flag fields to have whitespace after a '|'.
                if (copy.offset < parse.input.len and
                    parse.offset > 0 and
                    parse.input[parse.offset - 1] == '|')
                {
                    parse.offset = copy.offset;
                    continue;
                }

                break;
            }

            parse.offset += 1;
        }

        return parse.input[after_whitespace..parse.offset];
    }

    fn match_arg(
        out: *ObjectSyntaxTree,
        key_to_validate: []const u8,
        value_to_validate: []const u8,
    ) !void {
        inline for (@typeInfo(ObjectSyntaxTree).Union.fields) |object_syntax_tree_field| {
            if (std.mem.eql(u8, @tagName(out.*), object_syntax_tree_field.name)) {
                var active_value = @field(out, object_syntax_tree_field.name);
                const ActiveValueType = @TypeOf(active_value);

                inline for (@typeInfo(ActiveValueType).Struct.fields) |active_value_field| {
                    if (std.mem.eql(u8, active_value_field.name, key_to_validate)) {
                        // Handle everything but flags, skip reserved and timestamp.
                        if (comptime (!std.mem.eql(u8, active_value_field.name, "flags") and
                            !std.mem.eql(u8, active_value_field.name, "reserved") and
                            !std.mem.eql(u8, active_value_field.name, "timestamp")))
                        {
                            @field(
                                @field(out.*, object_syntax_tree_field.name),
                                active_value_field.name,
                            ) = try std.fmt.parseInt(
                                active_value_field.field_type,
                                value_to_validate,
                                10,
                            );
                        }

                        // Handle flags, specific to Account and Transfer fields.
                        if (comptime std.mem.eql(u8, active_value_field.name, "flags") and
                            !std.mem.eql(u8, object_syntax_tree_field.name, "id"))
                        {
                            var flags_to_validate = std.mem.split(u8, value_to_validate, "|");
                            var validated_flags = std.mem.zeroInit(active_value_field.field_type, .{});
                            while (flags_to_validate.next()) |flag_to_validate| {
                                var flag_without_whitespace_to_validate = std.mem.trim(
                                    u8,
                                    flag_to_validate,
                                    std.ascii.spaces[0..],
                                );
                                inline for (@typeInfo(
                                    active_value_field.field_type,
                                ).Struct.fields) |known_flag_field| {
                                    if (std.mem.eql(u8, known_flag_field.name, flag_without_whitespace_to_validate)) {
                                        if (comptime !std.mem.eql(
                                            u8,
                                            known_flag_field.name,
                                            "padding",
                                        )) {
                                            @field(validated_flags, known_flag_field.name) = true;
                                        }
                                    }
                                }
                            }
                            @field(@field(out.*, object_syntax_tree_field.name), "flags") = validated_flags;
                        }
                    }
                }
            }
        }
    }

    // Statement grammar parsed here.
    // STATEMENT: OPERATION ARGS [;]
    // OPERATION: create_accounts | lookup_accounts | create_transfers | lookup_transfers
    //      ARGS: ARG [, ARG]
    //       ARG: KEY = VALUE
    //       KEY: string
    //     VALUE: string [| VALUE]
    //
    // For example:
    //   create_accounts id=1 code=2 ledger=3, id = 2 code= 2 ledger =3;
    //   create_accounts flags=linked | debits_must_not_exceed_credits ;
    pub fn parse_statement(
        arena: *std.heap.ArenaAllocator,
        input: []const u8,
    ) (error{
        AccessDenied,
        BrokenPipe,
        ConnectionResetByPeer,
        DiskQuota,
        FileTooBig,
        InputOutput,
        LockViolation,
        NoSpaceLeft,
        NotOpenForWriting,
        OperationAborted,
        OutOfMemory,
        SystemResources,
        Unexpected,
        WouldBlock,
    } || Error)!StatementSyntaxTree {
        var args = std.ArrayList(*ObjectSyntaxTree).init(arena.allocator());

        var parse = &Parser{ .input = input };
        parse.eat_whitespace();
        const after_whitespace = parse.offset;
        const operation_identifier = parse.parse_identifier();

        const operation = operation: {
            if (std.meta.stringToEnum(Operation, operation_identifier)) |valid_operation| {
                break :operation valid_operation;
            }

            if (operation_identifier.len == 0) {
                break :operation .none;
            }

            // Set up the offset to after the whitespace so the
            // fail_at function points at where we actually expected the
            // token.
            parse.offset = after_whitespace;
            try parse.fail_at(
                \\Operation must be help, create_accounts, lookup_accounts,
                \\create_transfers, or lookup_transfers. Got: '{s}'.
                \\
            ,
                .{operation_identifier},
            );
            return Error.BadOperation;
        };

        const default: ObjectSyntaxTree = switch (operation) {
            .help, .none => {
                return StatementSyntaxTree{
                    .operation = operation,
                    .args = undefined,
                };
            },
            .create_accounts => .{ .account = std.mem.zeroInit(tb.Account, .{}) },
            .create_transfers => .{ .transfer = std.mem.zeroInit(tb.Transfer, .{}) },
            .lookup_accounts => .{ .id = .{ .id = 0 } },
            .lookup_transfers => .{ .id = .{ .id = 0 } },
        };
        var object = default;

        var has_fields = false;
        while (parse.offset < parse.input.len) {
            parse.eat_whitespace();
            // Always need to check `i` against length in case we've hit the end.
            if (parse.offset >= parse.input.len or parse.input[parse.offset] == ';') {
                break;
            }

            // Expect comma separating objects.
            if (parse.offset < parse.input.len and parse.input[parse.offset] == ',') {
                parse.offset += 1;
                var copy = try arena.allocator().create(ObjectSyntaxTree);
                copy.* = object;
                try args.append(copy);

                // Reset object.
                object = default;
                has_fields = false;
            }

            // Grab key.
            const id_result = parse.parse_identifier();

            if (id_result.len == 0) {
                try parse.fail_at(
                    "Expected key starting key-value pair. e.g. `id=1`\n",
                    .{},
                );
                return Error.BadIdentifier;
            }

            // Grab =.
            parse.parse_syntax_char('=') catch {
                try parse.fail_at(
                    "Expected equal sign after key '{s}' in key-value" ++
                        " pair. e.g. `id=1`.\n",
                    .{id_result},
                );
                return Error.MissingEqualBetweenKeyValuePair;
            };

            // Grab value.
            const value_result = parse.parse_value();

            if (value_result.len == 0) {
                try parse.fail_at(
                    "Expected value after equal sign in key-value pair. e.g. `id=1`.\n",
                    .{},
                );
                return Error.BadValue;
            }

            // Match key to a field in the struct.
            match_arg(&object, id_result, value_result) catch {
                try parse.fail_at(
                    "'{s}'='{s}' is not a valid pair for {s}.\n",
                    .{ id_result, value_result, @tagName(object) },
                );
                return Error.BadKeyValuePair;
            };

            has_fields = true;
        }

        // Add final object.
        if (has_fields) {
            var copy = try arena.allocator().create(ObjectSyntaxTree);
            copy.* = object;
            try args.append(copy);
        }

        return StatementSyntaxTree{
            .operation = operation,
            .args = args.items,
        };
    }
};

pub fn ReplType(comptime MessageBus: type) type {
    const Client = vsr.Client(StateMachine, MessageBus);

    return struct {
        event_loop_done: bool,
        request_done: bool,

        interactive: bool,
        debug_logs: bool,

        client: *Client,
        message: ?*MessagePool.Message,

        const Repl = @This();

        fn fail(repl: *const Repl, comptime fmt: []const u8, args: anytype) !void {
            if (!repl.interactive) {
                const stderr = std.io.getStdErr().writer();
                try stderr.print(fmt, args);
                std.os.exit(1);
            }

            try print(fmt, args);
        }

        fn debug(repl: *const Repl, comptime fmt: []const u8, args: anytype) !void {
            if (repl.debug_logs) {
                try print("[Debug] " ++ fmt, args);
            }
        }

        const Object = enum { account, transfer };
        fn do_statement(
            repl: *Repl,
            arena: *std.heap.ArenaAllocator,
            statement: Parser.StatementSyntaxTree,
        ) !void {
            try repl.debug("Running command: {}.\n", .{statement.operation});
            switch (statement.operation) {
                .none => {
                    // No input was parsed.
                    try repl.debug("No command was parsed, continuing.\n", .{});
                },
                .help => {
                    try display_help();
                    std.os.exit(0);
                },
                .create_accounts => try repl.create(
                    arena,
                    statement.args,
                    tb.Account,
                    Object.account,
                ),
                .lookup_accounts => try repl.lookup(
                    arena,
                    statement.args,
                    Object.account,
                ),
                .create_transfers => try repl.create(
                    arena,
                    statement.args,
                    tb.Transfer,
                    Object.transfer,
                ),
                .lookup_transfers => try repl.lookup(
                    arena,
                    statement.args,
                    Object.transfer,
                ),
            }
        }

        const single_repl_input_max = 10 * 4 * 1024;
        fn do_repl(
            repl: *Repl,
            arena: *std.heap.ArenaAllocator,
        ) !void {
            try print("> ", .{});

            const stdin = std.io.getStdIn();
            var stdin_buffered_reader = std.io.bufferedReader(stdin.reader());
            var stdin_stream = stdin_buffered_reader.reader();

            const input = stdin_stream.readUntilDelimiterOrEofAlloc(
                arena.allocator(),
                ';',
                single_repl_input_max,
            ) catch |err| {
                repl.event_loop_done = true;
                return err;
            } orelse {
                // EOF.
                repl.event_loop_done = true;
                try repl.fail("\nExiting.\n", .{});
                return;
            };

            const statement = Parser.parse_statement(arena, input) catch |err| {
                switch (err) {
                    // These are parsing errors, so the REPL should
                    // not continue to execute this statement but can
                    // still accept new statements.
                    Parser.Error.BadIdentifier,
                    Parser.Error.BadOperation,
                    Parser.Error.BadValue,
                    Parser.Error.BadKeyValuePair,
                    Parser.Error.MissingEqualBetweenKeyValuePair,
                    Parser.Error.NoSyntaxMatch,
                    => return,

                    // An unexpected error for which we do
                    // want the stacktrace.
                    error.AccessDenied,
                    error.BrokenPipe,
                    error.ConnectionResetByPeer,
                    error.DiskQuota,
                    error.FileTooBig,
                    error.InputOutput,
                    error.LockViolation,
                    error.NoSpaceLeft,
                    error.NotOpenForWriting,
                    error.OperationAborted,
                    error.OutOfMemory,
                    error.SystemResources,
                    error.Unexpected,
                    error.WouldBlock,
                    => return err,
                }
            };
            try repl.do_statement(
                arena,
                statement,
            );
        }

        fn display_help() !void {
            const version = build_options.git_tag orelse "experimental";
            try print("TigerBeetle CLI Client " ++ version ++ "\n" ++
                \\  Hit enter after a semicolon to run a command.
                \\
                \\Examples:
                \\  create_accounts id=1 code=10 ledger=700,
                \\                  id=2 code=10 ledger=700;
                \\  create_transfers id=1 debit_account_id=1 credit_account_id=2 amount=10 ledger=700 code=10;
                \\  lookup_accounts id=1;
                \\  lookup_accounts id=1, id=2;
                \\
                \\
            , .{});
        }

        pub fn run(
            arena: *std.heap.ArenaAllocator,
            addresses: []std.net.Address,
            cluster_id: u32,
            statements: []const u8,
            verbose: bool,
        ) !void {
            const allocator = arena.allocator();

            var repl = &Repl{
                .client = undefined,
                .message = null,
                .debug_logs = verbose,
                .request_done = true,
                .event_loop_done = false,
                .interactive = statements.len == 0,
            };

            try repl.debug("Connecting to '{s}'.\n", .{addresses});

            const client_id = std.crypto.random.int(u128);

            var io = try IO.init(32, 0);

            var message_pool = try MessagePool.init(allocator, .client);

            var client = try Client.init(
                allocator,
                client_id,
                cluster_id,
                @intCast(u8, addresses.len),
                &message_pool,
                .{
                    .configuration = addresses,
                    .io = &io,
                },
            );
            repl.client = &client;

            if (statements.len > 0) {
                var statements_iterator = std.mem.split(u8, statements, ";");
                while (statements_iterator.next()) |statement_string| {
                    // Release allocation after every execution.
                    var execution_arena = std.heap.ArenaAllocator.init(allocator);
                    defer execution_arena.deinit();
                    var statement = Parser.parse_statement(
                        &execution_arena,
                        statement_string,
                    ) catch |err| {
                        switch (err) {
                            // These are parsing errors and since this
                            // is not an interactive command, we should
                            // exit immediately. Parsing error info
                            // has already been emitted to stderr.
                            Parser.Error.BadIdentifier,
                            Parser.Error.BadOperation,
                            Parser.Error.BadValue,
                            Parser.Error.BadKeyValuePair,
                            Parser.Error.MissingEqualBetweenKeyValuePair,
                            Parser.Error.NoSyntaxMatch,
                            => std.os.exit(1),

                            // An unexpected error for which we do
                            // want the stacktrace.
                            error.AccessDenied,
                            error.BrokenPipe,
                            error.ConnectionResetByPeer,
                            error.DiskQuota,
                            error.FileTooBig,
                            error.InputOutput,
                            error.LockViolation,
                            error.NoSpaceLeft,
                            error.NotOpenForWriting,
                            error.OperationAborted,
                            error.OutOfMemory,
                            error.SystemResources,
                            error.Unexpected,
                            error.WouldBlock,
                            => return err,
                        }
                    };
                    try do_statement(repl, &execution_arena, statement);
                }
            } else {
                try display_help();
                std.os.exit(0);
            }

            while (!repl.event_loop_done) {
                if (repl.request_done and repl.interactive) {
                    // Release allocation after every execution.
                    var execution_arena = std.heap.ArenaAllocator.init(allocator);
                    defer execution_arena.deinit();
                    try repl.do_repl(&execution_arena);
                }
                repl.client.tick();
                try io.run_for_ns(constants.tick_ms * std.time.ns_per_ms);
            }
        }

        fn create(
            repl: *Repl,
            arena: *std.heap.ArenaAllocator,
            objects: []*Parser.ObjectSyntaxTree,
            comptime T: type,
            comptime object_type: Object,
        ) !void {
            if (objects.len == 0) {
                try repl.fail("No {s}s to create.\n", .{@tagName(object_type)});
                return;
            }

            var allocator = arena.allocator();
            var batch = try std.ArrayList(T).initCapacity(allocator, objects.len);

            for (objects) |object| {
                batch.appendAssumeCapacity(@field(object, @tagName(object_type)));
            }

            assert(batch.items.len == objects.len);

            // Submit batch.
            try repl.send(
                switch (object_type) {
                    .account => .create_accounts,
                    .transfer => .create_transfers,
                },
                std.mem.sliceAsBytes(batch.items),
            );
        }

        fn lookup(
            repl: *Repl,
            arena: *std.heap.ArenaAllocator,
            objects: []*Parser.ObjectSyntaxTree,
            object_type: Object,
        ) !void {
            if (objects.len == 0) {
                try repl.fail("No {s} to look up.\n", .{@tagName(object_type)});
                return;
            }

            var allocator = arena.allocator();
            var ids = try std.ArrayList(u128).initCapacity(allocator, objects.len);

            for (objects) |object| {
                try ids.append(object.id.id);
            }

            // Submit batch.
            try repl.send(
                switch (object_type) {
                    .account => .lookup_accounts,
                    .transfer => .lookup_transfers,
                },
                std.mem.sliceAsBytes(ids.items),
            );
        }

        fn send(
            repl: *Repl,
            operation: StateMachine.Operation,
            payload: []u8,
        ) !void {
            assert(repl.message == null);

            repl.request_done = false;
            repl.message = repl.client.get_message();

            stdx.copy_disjoint(
                .inexact,
                u8,
                repl.message.?.buffer[@sizeOf(vsr.Header)..],
                payload,
            );

            try repl.debug("Sending command: {}.\n", .{operation});
            repl.client.request(
                @intCast(u128, @ptrToInt(repl)),
                client_request_callback,
                operation,
                repl.message.?,
                payload.len,
            );
        }

        fn display_object(object: anytype) !void {
            assert(@TypeOf(object) == tb.Account or @TypeOf(object) == tb.Transfer);
            try print("{{\n", .{});
            inline for (@typeInfo(@TypeOf(object.*)).Struct.fields) |object_field, i| {
                if (comptime std.mem.eql(u8, object_field.name, "reserved")) {
                    continue;
                    // No need to print out reserved.
                }

                if (i > 0) {
                    try print(",\n", .{});
                }

                if (comptime std.mem.eql(u8, object_field.name, "flags")) {
                    try print("  \"" ++ object_field.name ++ "\": [", .{});
                    var needs_comma = false;

                    inline for (@typeInfo(object_field.field_type).Struct.fields) |flag_field| {
                        if (comptime !std.mem.eql(u8, flag_field.name, "padding")) {
                            if (@field(@field(object, "flags"), flag_field.name)) {
                                if (needs_comma) {
                                    try print(",", .{});
                                    needs_comma = false;
                                }

                                try print("\"{s}\"", .{flag_field.name});
                                needs_comma = true;
                            }
                        }
                    }

                    try print("]", .{});
                } else {
                    try print(
                        "  \"{s}\": \"{}\"",
                        .{ object_field.name, @field(object, object_field.name) },
                    );
                }
            }
            try print("\n}}\n", .{});
        }

        fn client_request_callback_error(
            user_data: u128,
            operation: StateMachine.Operation,
            result: []const u8,
        ) !void {
            const repl = @intToPtr(*Repl, @intCast(u64, user_data));
            assert(repl.request_done == false);
            try repl.debug("Operation completed: {}.\n", .{operation});

            defer {
                repl.request_done = true;
                repl.client.unref(repl.message.?);
                repl.message = null;

                if (!repl.interactive) {
                    repl.event_loop_done = true;
                }
            }

            switch (operation) {
                .create_accounts => {
                    const create_account_results = std.mem.bytesAsSlice(
                        tb.CreateAccountsResult,
                        result,
                    );

                    if (create_account_results.len > 0) {
                        for (create_account_results) |*reason| {
                            try print(
                                "Failed to create account ({}): {any}.\n",
                                .{ reason.index, reason.result },
                            );
                        }
                    }
                },
                .lookup_accounts => {
                    const lookup_account_results = std.mem.bytesAsSlice(
                        tb.Account,
                        result,
                    );

                    if (lookup_account_results.len == 0) {
                        try repl.fail("No such account or accounts exists.\n", .{});
                    } else {
                        for (lookup_account_results) |*account| {
                            try display_object(account);
                        }
                    }
                },
                .create_transfers => {
                    const create_transfer_results = std.mem.bytesAsSlice(
                        tb.CreateTransfersResult,
                        result,
                    );

                    if (create_transfer_results.len > 0) {
                        for (create_transfer_results) |*reason| {
                            try print(
                                "Failed to create transfer ({}): {any}.\n",
                                .{ reason.index, reason.result },
                            );
                        }
                    }
                },
                .lookup_transfers => {
                    const lookup_transfer_results = std.mem.bytesAsSlice(
                        tb.Transfer,
                        result,
                    );

                    if (lookup_transfer_results.len == 0) {
                        try repl.fail("No such transfer or transfers exists.\n", .{});
                    } else {
                        for (lookup_transfer_results) |*transfer| {
                            try display_object(transfer);
                        }
                    }
                },
            }
        }

        fn client_request_callback(
            user_data: u128,
            operation: StateMachine.Operation,
            result: []const u8,
        ) void {
            client_request_callback_error(
                user_data,
                operation,
                result,
            ) catch |err| {
                const repl = @intToPtr(*Repl, @intCast(u64, user_data));
                repl.fail("Error in callback: {any}", .{err}) catch return;
            };
        }
    };
}
