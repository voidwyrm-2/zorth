const std = @import("std");

const buildzon = @import("build.zig.zon");

const clap = @import("clap");

const Zorth = @import("Zorth.zig");

pub fn main() !u8 {
    var dba = std.heap.DebugAllocator(.{ .safety = true }).init;
    defer {
        if (dba.deinit() == .leak)
            std.debug.print("WARNING: leaks were detected\n\n\n\n", .{});
    }

    const allocator = dba.allocator();

    const stdout = std.io.getStdOut().writer();

    const stderr = std.io.getStdErr().writer();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help     Print the help message and exit.
        \\-v, --version  Print the current Zorth version.
        \\-s, --stack    Print the stack after execution.
        \\<str>...
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.report(stderr, err) catch {};
        return 1;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(stdout, clap.Help, &params, .{});
        return 0;
    }

    if (res.args.version != 0) {
        try stdout.print("Zorth interpreter version {s}\n", .{buildzon.version});
        return 0;
    }

    if (res.positionals[0].len == 0) {
        try stderr.print("no input files\n", .{});
        return 1;
    }

    var fileacc = std.ArrayList(u8).init(allocator);
    defer fileacc.deinit();

    const dir = std.fs.cwd();

    const maxSize = std.math.maxInt(usize);

    for (res.positionals[0]) |path| {
        const data = dir.readFileAlloc(allocator, path, maxSize) catch |err| {
            switch (err) {
                error.FileNotFound => {
                    try stderr.print("File '{s}' does not exist\n", .{path});
                },
                else => {
                    try stderr.print("Failed with error {s}\n", .{@errorName(err)});
                },
            }

            return err;
        };
        defer allocator.free(data);

        try fileacc.appendSlice(data);
    }

    var errstr: []const u8 = "";

    var interpreter = try Zorth.init(allocator, &errstr);
    defer interpreter.deinit();

    interpreter.execute(fileacc.items) catch |err| {
        if (err == error.Exit) // handle exit error
            return errstr[0];

        if (errstr.len > 0)
            try stdout.print("{s}\n", .{errstr});

        return err;
    };

    if (res.args.stack != 0)
        try stdout.print("{any}\n", .{interpreter.stack.list.items});

    return 0;
}
