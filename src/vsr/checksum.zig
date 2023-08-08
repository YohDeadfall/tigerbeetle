const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const mem = std.mem;
const testing = std.testing;

// TODO(king): Replace with std.crypto.auth.aegis.Aegis128LMac_128 once Zig is updated.
const Aegis128 = struct {
    const AesBlock = std.crypto.core.aes.Block;
    const State = [8]AesBlock;

    fn init(key: [16]u8, nonce: [16]u8) State {
        const c1 = AesBlock.fromBytes(&.{ 0xdb, 0x3d, 0x18, 0x55, 0x6d, 0xc2, 0x2f, 0xf1, 0x20, 0x11, 0x31, 0x42, 0x73, 0xb5, 0x28, 0xdd });
        const c2 = AesBlock.fromBytes(&.{ 0x0, 0x1, 0x01, 0x02, 0x03, 0x05, 0x08, 0x0d, 0x15, 0x22, 0x37, 0x59, 0x90, 0xe9, 0x79, 0x62 });
        const key_block = AesBlock.fromBytes(&key);
        const nonce_block = AesBlock.fromBytes(&nonce);
        var blocks = [8]AesBlock{
            key_block.xorBlocks(nonce_block),
            c1,
            c2,
            c1,
            key_block.xorBlocks(nonce_block),
            key_block.xorBlocks(c2),
            key_block.xorBlocks(c1),
            key_block.xorBlocks(c2),
        };
        var i: usize = 0;
        while (i < 10) : (i += 1) {
            update(&blocks, nonce_block, key_block);
        }
        return blocks;
    }

    inline fn update(blocks: *State, d1: AesBlock, d2: AesBlock) void {
        const tmp = blocks[7];
        comptime var i: usize = 7;
        inline while (i > 0) : (i -= 1) {
            blocks[i] = blocks[i - 1].encrypt(blocks[i]);
        }
        blocks[0] = tmp.encrypt(blocks[0]);
        blocks[0] = blocks[0].xorBlocks(d1);
        blocks[4] = blocks[4].xorBlocks(d2);
    }

    fn absorb(blocks: *State, src: *const [32]u8) void {
        const msg0 = AesBlock.fromBytes(src[0..16]);
        const msg1 = AesBlock.fromBytes(src[16..32]);
        update(blocks, msg0, msg1);
    }

    fn mac(blocks: *State, adlen: usize, mlen: usize) [16]u8 {
        var sizes: [16]u8 = undefined;
        mem.writeIntLittle(u64, sizes[0..8], adlen * 8);
        mem.writeIntLittle(u64, sizes[8..16], mlen * 8);
        const tmp = AesBlock.fromBytes(&sizes).xorBlocks(blocks[2]);
        var i: usize = 0;
        while (i < 7) : (i += 1) {
            update(blocks, tmp, tmp);
        }
        return blocks[0].xorBlocks(blocks[1]).xorBlocks(blocks[2]).xorBlocks(blocks[3]).xorBlocks(blocks[4])
            .xorBlocks(blocks[5]).xorBlocks(blocks[6]).toBytes();
    }

    inline fn hash(blocks: *State, source: []const u8) u128 {
        var i: usize = 0;
        while (i + 32 <= source.len) : (i += 32) {
            absorb(blocks, source[i..][0..32]);
        }
        if (source.len % 32 != 0) {
            var src: [32]u8 align(16) = mem.zeroes([32]u8);
            mem.copy(u8, src[0 .. source.len % 32], source[i .. i + source.len % 32]);
            absorb(blocks, &src);
        }
        // NB: Intentionally pass source.len as adlen, *not* as mlen.
        return @bitCast(u128, mac(blocks, source.len, 0));
    }
};

var seed_once = std.once(seed_init);
var seed_state: Aegis128.State = undefined;

fn seed_init() void {
    const key = mem.zeroes([16]u8);
    const nonce = mem.zeroes([16]u8);
    seed_state = Aegis128.init(key, nonce);
}

// Lazily initialize the Aegis State instead of recomputing it on each call to checksum().
// Then, make a copy of the state and use that to hash the source input bytes.
pub fn checksum(source: []const u8) u128 {
    seed_once.call();
    var state_copy = seed_state;
    return Aegis128.hash(&state_copy, source);
}

fn std_checksum(source: []const u8) u128 {
    // NB: For hashing purposes, we pass `source` to an AEAD scheme as AD (Associated Data).
    // This makes sense, because AD is supposed to be a known plaintext, which our `source` is for
    // the purpose of hashing.
    var c: [0]u8 = .{};
    var tag: [16]u8 = undefined;
    const m: [0]u8 = .{};
    const ad = source;
    const nonce: [16]u8 = [_]u8{0x00} ** 16;
    const key: [16]u8 = [_]u8{0x00} ** 16;

    std.crypto.aead.aegis.Aegis128L.encrypt(&c, &tag, &m, ad, nonce, key);
    return @bitCast(u128, tag);
}

test "Aegis test vectors" {
    const TestVector = struct {
        source: []const u8,
        hash: u128,
    };

    for (&[_]TestVector{
        .{
            .source = &[_]u8{0x00} ** 16,
            .hash = @byteSwap(@as(u128, 0xf72ad48dd05dd1656133101cd4be3a26)),
        },
        .{
            .source = &[_]u8{},
            .hash = @byteSwap(@as(u128, 0x83cc600dc4e3e7e62d4055826174f149)),
        },
    }) |test_vector| {
        try testing.expectEqual(test_vector.hash, checksum(test_vector.source));
        try testing.expectEqual(test_vector.hash, std_checksum(test_vector.source));
    }
}

test "Aegis simple fuzzing" {
    var prng = std.rand.DefaultPrng.init(42);

    const msg_min = 1;
    const msg_max = 1 * 1024 * 1024;

    var msg_buf = try testing.allocator.alloc(u8, msg_max);
    defer testing.allocator.free(msg_buf);

    var i: usize = 0;
    while (i < 1_000) : (i += 1) {
        const msg_len = prng.random().intRangeAtMostBiased(usize, msg_min, msg_max);
        const msg = msg_buf[0..msg_len];
        prng.fill(msg);

        // Make sure it matches with stdlib.
        const msg_checksum = checksum(msg);
        try testing.expectEqual(msg_checksum, std_checksum(msg));

        // Sanity check that it's a pure function.
        const msg_checksum_again = checksum(msg);
        try testing.expectEqual(msg_checksum, msg_checksum_again);

        // Change the message and make sure the checksum changes.
        const random_byte = prng.random().uintLessThan(usize, msg.len);
        const random_bit = @shlExact(@as(u8, 1), prng.random().int(u3));
        msg[random_byte] ^= random_bit;
        const changed_checksum = checksum(msg);
        try testing.expectEqual(changed_checksum, std_checksum(msg));
        try testing.expect(changed_checksum != msg_checksum);

        // Further, test avalanching -- flipping a single bit should randomly flip
        // all outputs bits, so we expect about half of the bits to stay the same,
        // and a big deviation from that would be surprising.
        const different_bits = @popCount(changed_checksum ^ msg_checksum);
        try testing.expect(different_bits > 32);
        try testing.expect(different_bits < 128 - 32);
    }
}

// Change detector test to ensure we don't inadvertency modify our checksum function.
test "Aegis stability" {
    var buf: [1024]u8 = undefined;
    var cases: [896]u128 = undefined;
    var case_index: usize = 0;

    // Zeros of various lengths.
    var subcase: usize = 0;
    while (subcase < 128) : (subcase += 1) {
        const message = buf[0..subcase];
        std.mem.set(u8, message, 0);

        cases[case_index] = checksum(message);
        case_index += 1;
    }

    // 64 bytes with exactly one bit set.
    subcase = 0;
    while (subcase < 64 * 8) : (subcase += 1) {
        const message = buf[0..64];
        std.mem.set(u8, message, 0);
        message[@divFloor(subcase, 8)] = @shlExact(@as(u8, 1), @intCast(u3, subcase % 8));

        cases[case_index] = checksum(message);
        case_index += 1;
    }

    // Pseudo-random data from a specific PRNG of various lengths.
    var prng = std.rand.Xoshiro256.init(92);
    subcase = 0;
    while (subcase < 256) : (subcase += 1) {
        const message = buf[0 .. subcase + 13];
        prng.fill(message);

        cases[case_index] = checksum(message);
        case_index += 1;
    }

    // Sanity check that we are not getting trivial answers.
    for (cases) |case_a, i| {
        assert(case_a != 0);
        assert(case_a != std.math.maxInt(u128));
        for (cases[0..i]) |case_b| assert(case_a != case_b);
    }

    // Hash me, baby, one more time! If this final hash changes, we broke compatibility in a major
    // way.
    comptime assert(builtin.target.cpu.arch.endian() == .Little);
    const hash = checksum(std.mem.sliceAsBytes(&cases));
    try testing.expectEqual(hash, 0x82dcaacf4875b279446825b6830d1263);
}
