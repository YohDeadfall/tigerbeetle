const builtin = @import("builtin");
const std = @import("std");

const assert = std.debug.assert;

const Docs = @import("../docs_types.zig").Docs;
const binary_filename = @import("../shutil.zig").binary_filename;
const run_shell = @import("../shutil.zig").run_shell;
const path_separator = @import("../shutil.zig").path_separator;

fn current_commit_post_install_hook(
    arena: *std.heap.ArenaAllocator,
    sample_directory: []const u8,
    root: []const u8,
) !void {
    try std.os.chdir(root);
    try run_shell(
        arena,
        try std.fmt.allocPrint(
            arena.allocator(),
            "{s} build dotnet_client",
            .{try binary_filename(arena, &[_][]const u8{ "zig", "zig" })},
        ),
    );

    try std.os.chdir(sample_directory);
    // When running integration tests, the integration tests already
    // have a .csproj file. But this will jostle with the .csproj
    // created during the prepare_directory step when we call `dotnet
    // new console`. So we'll get rid of the existing .csproj file
    // before we call `dotnet new`.
    var path_parts_backwards = std.mem.splitBackwards(
        u8,
        sample_directory,
        path_separator,
    );
    const directory_name = path_parts_backwards.next().?;
    run_shell(arena, try std.fmt.allocPrint(
        arena.allocator(),
        "rm {s}.csproj",
        .{directory_name},
    )) catch {
        // Ok if there is no csproj (the client_docs scenario).
    };

    // Find the .csproj file so we can swap out the public package
    // with our local build, if the .csproj file exists.
    var dir = try std.fs.cwd().openIterableDir(".", .{});
    defer dir.close();

    var walker = try dir.walk(arena.allocator());
    defer walker.deinit();

    const csproj_filename = blk: {
        while (try walker.next()) |entry| {
            if (std.mem.endsWith(u8, entry.path, ".csproj")) {
                break :blk entry.path;
            }
        }

        return error.CSProjFileNotFound;
    };

    const public_reference =
        \\  <ItemGroup>
        \\    <PackageReference Include="tigerbeetle" Version="0.13.88" />
        \\  </ItemGroup>
    ;
    const old_csproj_contents = try std.fs.cwd().readFileAlloc(arena.allocator(), csproj_filename, 1024 * 10);
    assert(std.mem.containsAtLeast(u8, old_csproj_contents, 1, public_reference));

    const local_reference = try std.fmt.allocPrint(
        arena.allocator(),
        \\  <ItemGroup>
        \\    <ProjectReference Include="{s}/src/clients/dotnet/TigerBeetle/TigerBeetle.csproj" />
        \\  </ItemGroup>
        \\  <ItemGroup>
        \\    <Content Include="{s}/src/clients/dotnet/TigerBeetle/runtimes/$(RuntimeIdentifier)/native/*.*">
        \\      <CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
        \\    </Content>
        \\  </ItemGroup>
    ,
        .{ root, root },
    );
    const csproj_contents = try std.mem.replaceOwned(
        u8,
        arena.allocator(),
        old_csproj_contents,
        public_reference,
        local_reference,
    );

    try std.fs.cwd().writeFile(
        csproj_filename,
        csproj_contents,
    );
}

pub const DotnetDocs = Docs{
    .directory = "dotnet",

    .markdown_name = "cs",
    .extension = "cs",
    .proper_name = ".NET",

    .test_source_path = "",

    .name = "tigerbeetle-dotnet",
    .description = 
    \\The TigerBeetle client for .NET.
    ,

    .prerequisites = 
    \\* .NET >= 2.1
    \\And if you do not already have NuGet.org as a package
    \\source, make sure to add it:
    \\
    \\```console
    \\$ dotnet nuget add source https://api.nuget.org/v3/index.json -n nuget.org
    \\```
    ,

    .project_file_name = "",
    .project_file = "",

    .test_file_name = "Program",

    .install_prereqs = "",
    .install_sample_file = 
    \\using System;
    \\
    \\using TigerBeetle;
    \\
    \\// Validate import works.
    \\var id = new TigerBeetle.UInt128(1);
    \\Console.WriteLine("SUCCESS");
    ,

    .current_commit_pre_install_hook = null,
    .current_commit_post_install_hook = current_commit_post_install_hook,

    .install_commands = 
    \\dotnet new console
    \\dotnet add package tigerbeetle
    ,
    .build_commands = 
    \\dotnet restore
    \\dotnet clean --verbosity minimal
    \\dotnet build
    ,
    .run_commands = "dotnet run",

    .current_commit_install_commands_hook = null,
    .current_commit_build_commands_hook = null,
    .current_commit_run_commands_hook = null,

    .install_documentation = "",

    .examples = "",

    .client_object_example = 
    \\var tbAddress = Environment.GetEnvironmentVariable("TB_ADDRESS");
    \\var client = new Client(
    \\  clusterID: 0,
    \\  addresses: new[] {tbAddress != null ? tbAddress : "3000"}
    \\);
    ,

    .client_object_documentation = 
    \\If you create a `Client` like this, don't forget to call
    \\`client.Dispose()` when you are done with it. Otherwise you
    \\can use the `using` syntax:
    \\```csharp
    \\using (var client = new Client(...)) {
    \\  // Use client
    \\}
    \\```
    \\
    \\The `Client` class is thread-safe and for better performance, a
    \\single instance should be shared between multiple concurrent
    \\tasks. Multiple clients can be instantiated in case of connecting
    \\to more than one TigerBeetle cluster.
    ,

    .create_accounts_example = 
    \\var accounts = new[] {
    \\  new Account
    \\  {
    \\    Id = 137,
    \\    UserData = Guid.NewGuid(),
    \\    Ledger = 1,
    \\    Code = 718,
    \\    Flags = AccountFlags.None,
    \\  },
    \\};
    \\
    \\var createAccountsError = client.CreateAccounts(accounts);
    ,

    .create_accounts_documentation = 
    \\All TigerBeetle's IDs are 128-bit integers, and the .NET client
    \\accepts a wide range of values: `int`, `uint`, `long`, `ulong`,
    \\`Guid`, `byte[]` and `TigerBeetle.UInt128`.
    ,

    .account_flags_documentation = 
    \\To toggle behavior for an account, combine enum values stored in the
    \\`AccountFlags` object with bitwise-or:
    \\
    \\* `AccountFlags.None`
    \\* `AccountFlags.Linked`
    \\* `AccountFlags.DebitsMustNotExceedCredits`
    \\* `AccountFlags.CreditsMustNotExceedDebits`
    ,

    .account_flags_example = 
    \\var account0 = new Account{ /* ... account values ... */ };
    \\var account1 = new Account{ /* ... account values ... */ };
    \\account0.Flags = AccountFlags.Linked;
    \\
    \\createAccountsError = client.CreateAccounts(new []{account0, account1});
    ,

    .create_accounts_errors_example = 
    \\var account2 = new Account{ /* ... account values ... */ };
    \\var account3 = new Account{ /* ... account values ... */ };
    \\var account4 = new Account{ /* ... account values ... */ };
    \\
    \\createAccountsError = client.CreateAccounts(new []{account2, account3, account4});
    \\foreach (var error in createAccountsError) {
    \\	Console.WriteLine("Error creating account {0}: {1}", error.Index, error.Result);
    \\	return;
    \\}
    ,

    .create_accounts_errors_documentation = "",

    .lookup_accounts_example = 
    \\accounts = client.LookupAccounts(new TigerBeetle.UInt128[] { 137, 138 });
    ,

    .create_transfers_example = 
    \\var transfers = new[] {
    \\  new Transfer
    \\  {
    \\    Id = 1,
    \\    DebitAccountId = 1,
    \\    CreditAccountId = 2,
    \\    Timeout = 0,
    \\    UserData = 2,
    \\    Ledger = 1,
    \\    Code = 1,
    \\    Flags = 0,
    \\    Amount = 10,
    \\  }
    \\};
    \\
    \\var createTransfersError = client.CreateTransfers(transfers);
    ,

    .create_transfers_documentation = "",

    .create_transfers_errors_example = 
    \\foreach (var error in createTransfersError) {
    \\  Console.WriteLine("Error creating account {0}: {1}", error.Index, error.Result);
    \\  return;
    \\}
    ,

    .create_transfers_errors_documentation = "",

    .no_batch_example = 
    \\foreach(var t in transfers) {
    \\  createTransfersError = client.CreateTransfers(new []{t});
    \\  // error handling omitted
    \\}
    ,

    .batch_example = 
    \\var BATCH_SIZE = 8191;
    \\for (int i = 0; i < transfers.Length; i += BATCH_SIZE) {
    \\  var batchSize = BATCH_SIZE;
    \\  if (i + BATCH_SIZE > transfers.Length) {
    \\    batchSize = transfers.Length - i;
    \\  }
    \\  var segment = new ArraySegment<Transfer>(transfers, i, batchSize);
    \\  createTransfersError = client.CreateTransfers(segment.Array);
    \\  // error handling omitted
    \\}
    ,

    .transfer_flags_documentation = 
    \\To toggle behavior for an account, combine enum values stored in the
    \\`TransferFlags` object with bitwise-or:
    \\
    \\* `TransferFlags.None`
    \\* `TransferFlags.Linked`
    \\* `TransferFlags.Pending`
    \\* `TransferFlags.PostPendingTransfer`
    \\* `TransferFlags.VoidPendingTransfer`
    ,

    .transfer_flags_link_example = 
    \\var transfer0 = new Transfer{ /* ... account values ... */ };
    \\var transfer1 = new Transfer{ /* ... account values ... */ };
    \\transfer0.Flags = TransferFlags.Linked;
    \\createTransfersError = client.CreateTransfers(new Transfer[] {transfer0, transfer1});
    ,

    .transfer_flags_post_example = 
    \\var transfer = new Transfer
    \\{
    \\  Id = 2,
    \\  PendingId = 1,
    \\  Flags = TransferFlags.PostPendingTransfer,
    \\};
    \\createTransfersError = client.CreateTransfers(new Transfer[] {transfer});
    \\// error handling omitted
    ,

    .transfer_flags_void_example = 
    \\transfer = new Transfer
    \\{
    \\  Id = 2,
    \\  PendingId = 1,
    \\  Flags = TransferFlags.PostPendingTransfer,
    \\};
    \\createTransfersError = client.CreateTransfers(new Transfer[] {transfer});
    \\// error handling omitted
    ,

    .lookup_transfers_example = 
    \\transfers = client.LookupTransfers(new TigerBeetle.UInt128[] {1, 2});
    ,

    .linked_events_example = 
    \\var batch = new System.Collections.Generic.List<Transfer>();
    \\
    \\// An individual transfer (successful):
    \\batch.Add(new Transfer{Id = 1, /* ... rest of transfer ... */ });
    \\
    \\// A chain of 4 transfers (the last transfer in the chain closes the chain with linked=false):
    \\batch.Add(new Transfer{Id = 2, /* ... rest of transfer ... */ Flags = TransferFlags.Linked }); // Commit/rollback.
    \\batch.Add(new Transfer{Id = 3, /* ... rest of transfer ... */ Flags = TransferFlags.Linked }); // Commit/rollback.
    \\batch.Add(new Transfer{Id = 2, /* ... rest of transfer ... */ Flags = TransferFlags.Linked }); // Fail with exists
    \\batch.Add(new Transfer{Id = 4, /* ... rest of transfer ... */ }); // Fail without committing
    \\
    \\// An individual transfer (successful):
    \\// This should not see any effect from the failed chain above.
    \\batch.Add(new Transfer{Id = 2, /* ... rest of transfer ... */ });
    \\
    \\// A chain of 2 transfers (the first transfer fails the chain):
    \\batch.Add(new Transfer{Id = 2, /* ... rest of transfer ... */ Flags = TransferFlags.Linked });
    \\batch.Add(new Transfer{Id = 3, /* ... rest of transfer ... */ });
    \\
    \\// A chain of 2 transfers (successful):
    \\batch.Add(new Transfer{Id = 3, /* ... rest of transfer ... */ Flags = TransferFlags.Linked });
    \\batch.Add(new Transfer{Id = 4, /* ... rest of transfer ... */ });
    \\
    \\createTransfersError = client.CreateTransfers(batch.ToArray());
    \\// error handling omitted
    ,

    .developer_setup_documentation = "",

    .developer_setup_sh_commands = 
    \\cd src/clients/dotnet
    \\dotnet restore
    \\dotnet clean --verbosity minimal
    \\dotnet build
    \\if [ "$TEST" = "true" ]; then dotnet test; else echo "Skipping client unit tests"; fi
    ,

    .developer_setup_pwsh_commands = 
    \\cd src/clients/dotnet
    \\dotnet restore
    \\dotnet clean --verbosity minimal
    \\dotnet build
    \\if ($env:TEST -eq 'true') { dotnet test } else { echo "Skipping client unit test" }
    ,

    .test_main_prefix = 
    \\using System;
    \\using System.Diagnostics;
    \\
    \\using TigerBeetle;
    \\
    ,

    .test_main_suffix = "",
};
