const std = @import("std");
const Allocator = std.mem.Allocator;
const Arraylist = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Self = @This();

pub const Error = error{
    Exit,
    WordNotFound,
    WordError,
    StackUnderflow,
    UnexpectedStackOperands,
    NativeFunctionError,
};

const ZorthStack = struct {
    list: Arraylist(Value),
    err: *[]const u8,

    fn init(allocator: Allocator, err: *[]const u8) ZorthStack {
        return .{
            .list = Arraylist(Value).init(allocator),
            .err = err,
        };
    }

    fn deinit(self: *ZorthStack) void {
        self.list.deinit();
    }

    fn push(self: *ZorthStack, value: Value) !void {
        try self.list.append(value);
    }

    fn pop(self: *ZorthStack) Error!Value {
        return self.list.pop() orelse error.StackUnderflow;
    }

    fn expect(self: *ZorthStack, state: *Self, kinds: []const []const ValueKind) !void {
        var i = self.list.items.len - 1;

        if (self.list.items.len < kinds.len) {
            try state.errf("expected {d} items on the stack, but found {d} items instead", .{ kinds.len, self.list.items.len });
            return error.StackUnderflow;
        }

        for (kinds) |k| {
            const item = self.list.items[i];

            if (!item.isAny(k)) {
                var str = Arraylist(u8).init(state.arena.allocator());

                try str.appendSlice("expected type");

                if (k.len > 1)
                    try str.append('s');

                try str.append(' ');

                for (k, 0..) |vk, j| {
                    if (k.len > 1 and j == k.len - 1)
                        break;

                    try str.appendSlice(@tagName(vk));
                    try str.appendSlice(", ");
                }

                if (k.len > 1) {
                    if (k.len == 2)
                        str.shrinkAndFree(str.items.len - 2);

                    try str.appendSlice("or ");
                    try str.appendSlice(@tagName(k[k.len - 1]));
                    try str.appendSlice(", ");
                }

                try str.appendSlice("but found ");
                try str.appendSlice(@tagName(item.kind()));
                try str.appendSlice(" instead");

                try state.errs(str.items);

                return error.UnexpectedStackOperands;
            }

            if (i == 0)
                break;

            i -= 1;
        }
    }
};

const ZorthReader = struct {
    parent: ?*ZorthReader = null,
    idx: usize = 0,
    col: usize = 1,
    ln: usize = 1,
    first: bool = true,
    can_continue: bool = true,
    current_snippet: ?Snippet = null,
    last_snippet: ?Snippet = null,
    text: []const u8 = "",

    fn enter(self: *ZorthReader, allocator: Allocator, text: []const u8) !void {
        if (self.text.len > 0) {
            const parent = try allocator.create(ZorthReader);
            parent.* = self.*;

            self.* = .{
                .parent = parent,
            };
        }

        self.text = text;
    }

    fn escape(self: *ZorthReader, allocator: Allocator) void {
        if (self.parent) |parent| {
            self.* = parent.*;
            allocator.destroy(parent);
        }
    }

    fn advance(self: *ZorthReader) bool {
        if (self.first) {
            self.first = false;
        } else {
            self.idx += 1;
            self.col += 1;
        }

        self.can_continue = self.char() != null;

        if (self.can_continue) {
            if (self.char().? == '\n') {
                self.col = 1;
                self.ln += 1;
            }
        }

        return self.can_continue;
    }

    fn isDelimiter(self: *ZorthReader) bool {
        const ch = self.char() orelse return true;
        return ch < 33 or ch > 126;
    }

    fn char(self: *ZorthReader) ?u8 {
        return if (self.idx < self.text.len) self.text[self.idx] else null;
    }

    fn peek(self: *ZorthReader) ?u8 {
        return if (self.idx + 1 < self.text.len) self.text[self.idx + 1] else null;
    }

    fn transfer(self: *ZorthReader, snip: *const Snippet) void {
        self.last_snippet = self.current_snippet;
        self.current_snippet = snip.*;
    }

    fn skipWhitespace(self: *ZorthReader) void {
        if (!self.isDelimiter())
            return;

        while (self.advance() and self.isDelimiter()) {}
    }

    fn nextSnippet(self: *ZorthReader) ?Snippet {
        const startcol = self.col;
        const startln = self.ln;
        const start = self.idx;

        while (self.advance()) {
            if (self.isDelimiter() and self.idx - start > 0) {
                const s: Snippet = .{
                    .text = self.text[start..self.idx],
                    .col = startcol,
                    .ln = startln,
                    .range = .{ .start = start, .end = self.idx },
                };

                self.transfer(&s);

                return s;
            }
        }

        if (self.idx - start > 0 and start < self.text.len) {
            var s: Snippet = .{
                .text = self.text[start..],
                .col = startcol,
                .ln = startln,
                .range = .{ .start = start, .end = undefined },
            };

            s.range.end = s.range.start + s.text.len;

            self.transfer(&s);

            return s;
        }

        return null;
    }

    fn errf(self: *ZorthReader, allocator: Allocator, comptime fmt: []const u8, args: anytype) ![]const u8 {
        const msg = try std.fmt.allocPrint(allocator, fmt, args);
        return try self.err(allocator, msg);
    }

    fn err(self: *ZorthReader, allocator: Allocator, str: []const u8) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "error on line: {d}, col {d}: {s}", .{ self.ln, self.col, str });
    }

    pub fn format(
        self: ZorthStack,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{any}", .{self.list.items});
    }
};

const Snippet = struct {
    text: []const u8,
    col: usize,
    ln: usize,
    range: struct { start: usize, end: usize },

    pub fn format(
        self: Snippet,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print(
            "<`{s}` {d} {d} {d}..{d}>",
            .{
                self.text,
                self.col,
                self.ln,
                self.range.start,
                self.range.end,
            },
        );
    }
};

const WordKind = enum {
    native,
    composite,
};

const Word = union(WordKind) {
    native: *const fn (state: *Self) anyerror!void,
    composite: []const u8,

    pub fn format(
        self: Word,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .native => try writer.print("<native word>", .{}),
            .composite => |v| try writer.print("<composite word: '{s}'>", .{v}),
        }
    }
};

const ValueKind = enum {
    byte,
    int,
    float,
    ptr,
};

const Value = union(ValueKind) {
    byte: u8,
    int: isize,
    float: f32,
    ptr: [*]Value,

    fn kind(self: Value) ValueKind {
        return switch (self) {
            .byte => .byte,
            .int => .int,
            .float => .float,
            .ptr => .ptr,
        };
    }

    fn isAny(self: Value, opt: []const ValueKind) bool {
        for (opt) |k| {
            if (self == k)
                return true;
        }

        return false;
    }

    fn cast(self: Value, to: ValueKind) Value {
        return switch (to) {
            .byte => .{ .byte = switch (self) {
                .byte => |v| v,
                .int => |v| @truncate(@as(usize, @bitCast(v))),
                .float => |v| @intFromFloat(v),
                .ptr => |v| @truncate(@intFromPtr(v)),
            } },
            .int => .{ .int = switch (self) {
                .byte => |v| v,
                .int => |v| v,
                .float => |v| @intFromFloat(v),
                .ptr => |v| @intCast(@intFromPtr(v)),
            } },
            .float => .{ .float = switch (self) {
                .byte => |v| @floatFromInt(v),
                .int => |v| @floatFromInt(v),
                .float => |v| v,
                .ptr => |v| @floatFromInt(@as(usize, @intFromPtr(v))),
            } },
            .ptr => .{ .ptr = switch (self) {
                .byte => |v| @ptrFromInt(v),
                .int => |v| @ptrFromInt(@as(usize, @bitCast(v))),
                .float => |v| {
                    const i: i32 = @intFromFloat(v);
                    const u: u32 = @bitCast(i);
                    const s: usize = u;
                    return .{ .ptr = @ptrFromInt(s) };
                },
                .ptr => |v| v,
            } },
        };
    }

    pub fn format(
        self: Value,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .byte => |v| try writer.print("{d}", .{v}),
            .int => |v| try writer.print("{d}", .{v}),
            .float => |v| try writer.print("{d}", .{v}),
            .ptr => |v| try writer.print("&{x}", .{@intFromPtr(v)}),
        }
    }
};

pub const Builtins = struct {
    pub const Meta = struct {
        pub fn defineWord(state: *Self) anyerror!void {
            var ended = false;
            var name: ?Snippet = null;

            while (state.reader.advance()) {
                const s = state.reader.nextSnippet() orelse @panic("no next snippet");

                if (std.mem.eql(u8, s.text, ":")) {
                    try state.errs("':' inside word definition");
                    return error.WordError;
                } else if (std.mem.eql(u8, s.text, ";")) {
                    ended = true;
                    break;
                }

                if (name == null) {
                    name = s;
                }
            }

            if (!ended) {
                try state.errs("unterminated word definition");
                return error.WordError;
            } else if (name == null) {
                try state.errs("cannot use zero-length string as a word name");
                return error.WordError;
            }

            const snip = state.reader.text[name.?.range.end + 1 .. state.reader.idx - 2];
            //std.debug.print("name: '{s}', snip: '{s}'\n", .{ name.?.text, snip });

            try state.dict.put(name.?.text, .{ .composite = snip });

            //std.debug.print("'{s}': {s}\n", .{ name.?.text, state.dict.get(name.?.text) orelse @panic("not found") });
        }

        pub fn multilineComment(state: *Self) anyerror!void {
            while (state.reader.advance()) {
                const s = state.reader.nextSnippet() orelse @panic("no next snippet");

                if (std.mem.eql(u8, s.text, ")"))
                    return;
            }

            try state.errs("unterminated comment");
            return error.WordError;
        }

        pub fn ifelse(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
            });

            var start = state.reader.idx;
            var if_block: ?[]const u8 = null;
            var else_block: ?[]const u8 = null;
            var nest: usize = 0;
            var ended = false;

            while (state.reader.advance()) {
                const s = state.reader.nextSnippet() orelse @panic("no next snippet");

                if (std.mem.eql(u8, s.text, "if")) {
                    nest += 1;
                } else if (std.mem.eql(u8, s.text, "else") and nest == 0) {
                    if_block = state.reader.text[start + 1 .. state.reader.idx - 5];
                    start = state.reader.idx;
                } else if (std.mem.eql(u8, s.text, "then")) {
                    if (nest > 0) {
                        nest -= 1;
                    } else {
                        ended = true;

                        const block = state.reader.text[start + 1 .. state.reader.idx - 5];

                        if (if_block == null) {
                            if_block = block;
                        } else {
                            else_block = block;
                        }
                    }
                }
            }

            if (!ended) {
                try state.errs("unterminated if statement");
                return error.WordError;
            }

            const cond = (try state.stack.pop())
                .cast(.int)
                .int;

            //std.debug.print("cond: {d}, if: '{s}', else: '{s}'\n", .{ cond, if_block orelse "", else_block orelse "" });

            if (cond != 0) {
                try state.execute(if_block.?);
            } else if (else_block) |block| {
                try state.execute(block);
            }
        }
    };

    pub const Stack = struct {
        pub fn dup(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
            });

            const val = try state.stack.pop();

            try state.stack.push(val);
            try state.stack.push(val);
        }

        pub fn swap(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
                &.{ .byte, .int, .float, .ptr },
            });

            const a = try state.stack.pop();
            const b = try state.stack.pop();

            try state.stack.push(a);
            try state.stack.push(b);
        }

        pub fn drop(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
            });

            _ = try state.stack.pop();
        }
    };

    pub const Math = struct {
        const Ops = struct {
            fn add(x: f32, y: f32) anyerror!f32 {
                return x + y;
            }

            fn sub(x: f32, y: f32) anyerror!f32 {
                return x - y;
            }

            fn mul(x: f32, y: f32) anyerror!f32 {
                return x * y;
            }

            fn div(x: f32, y: f32) anyerror!f32 {
                return x / y;
            }

            fn rem(x: f32, y: f32) anyerror!f32 {
                return @mod(x, y);
            }

            fn pow(x: f32, y: f32) anyerror!f32 {
                return std.math.pow(f32, x, y);
            }
        };

        fn mathOp(state: *Self, op: *const fn (x: f32, y: f32) anyerror!f32) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
                &.{ .byte, .int, .float, .ptr },
            });

            const b = (try state.stack.pop())
                .cast(.float);
            const a = (try state.stack.pop())
                .cast(.float);

            const result: f32 = try op(a.float, b.float);

            const f: Value = .{
                .float = result,
            };

            try state.stack.push(f.cast(a.kind()));
        }

        pub fn add(state: *Self) anyerror!void {
            try mathOp(state, Ops.add);
        }

        pub fn sub(state: *Self) anyerror!void {
            try mathOp(state, Ops.sub);
        }

        pub fn mul(state: *Self) anyerror!void {
            try mathOp(state, Ops.mul);
        }

        pub fn div(state: *Self) anyerror!void {
            try mathOp(state, Ops.div);
        }

        pub fn rem(state: *Self) anyerror!void {
            try mathOp(state, Ops.rem);
        }

        pub fn pow(state: *Self) anyerror!void {
            try mathOp(state, Ops.pow);
        }
    };

    pub const IO = struct {
        pub fn out(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
            });

            var stdout = std.io.getStdOut().writer();

            const val = try state.stack.pop();

            try stdout.print("{s}", .{val});
        }

        pub fn emit(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int },
            });

            var stdout = std.io.getStdOut().writer();

            const val = try state.stack.pop();

            switch (val) {
                .byte => |v| try stdout.print("{c}", .{v}),
                .int => |v| {
                    const u: usize = @bitCast(v);
                    const char: u8 = @truncate(u);

                    try stdout.print("{c}", .{char});
                },
                else => unreachable,
            }
        }

        pub const cr = "10 emit";

        pub const print = ". cr";

        pub fn printString(state: *Self) anyerror!void {
            state.reader.skipWhitespace();

            const start = state.reader.idx;
            var ended = false;

            while (state.reader.advance()) {
                const ch = state.reader.char() orelse @panic("no char");

                if (state.reader.peek()) |nch| {
                    if (ch == '\\' and nch == '"') {
                        _ = state.reader.advance();
                        continue;
                    }
                }

                if (ch == '"') {
                    ended = true;
                    break;
                }
            }

            if (!ended) {
                try state.errs("unterminated string");
                return error.WordError;
            }

            var stdout = std.io.getStdOut().writer();
            try stdout.print("{s}", .{state.reader.text[start..state.reader.idx]});
            _ = state.reader.advance();
        }

        pub fn puts(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{.ptr},
            });

            var stdout = std.io.getStdOut().writer();

            const val = try state.stack.pop();

            const str: [*:0]u8 = @ptrCast(val.ptr);

            try stdout.print("{s}", .{str});
        }
    };

    pub const Ptr = struct {
        pub fn ref(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{ .byte, .int, .float, .ptr },
            });

            const val = try state.stack.pop();

            const ptr = try state.allocator.alloc(Value, 1);
            try state.managed_values.append(ptr);

            ptr[0] = val;

            try state.stack.push(.{ .ptr = ptr.ptr });
        }

        pub fn deref(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{.ptr},
            });

            const val = try state.stack.pop();

            try state.stack.push(val.ptr[0]);
        }

        pub fn alloc(state: *Self) anyerror!void {
            try state.stack.expect(state, &.{
                &.{.int},
            });

            const val = try state.stack.pop();
            const u: usize = @bitCast(val.int);
            const size: usize = @truncate(u);

            const arr = try state.allocator.alloc(Value, size);
            try state.managed_values.append(arr);

            try state.stack.push(.{ .ptr = arr.ptr });
        }

        // unfortunately, raw single- and muli- pointers can't be easily freed in the same way C's can
        // so they'll have to be automatically managed
        //pub fn free(state: *Self) anyerror!void {
        //    try state.stack.expect(state, &.{
        //        &.{.int},
        //    });
        //}

        pub fn createString(state: *Self) anyerror!void {
            state.reader.skipWhitespace();

            const start = state.reader.idx;
            var ended = false;

            while (state.reader.advance()) {
                const ch = state.reader.char() orelse @panic("no char");

                if (state.reader.peek()) |nch| {
                    if (ch == '\\' and nch == '"') {
                        _ = state.reader.advance();
                        continue;
                    }
                }

                if (ch == '"') {
                    ended = true;
                    break;
                }
            }

            if (!ended) {
                try state.errs("unterminated string");
                return error.WordError;
            }

            const lit = state.reader.text[start..state.reader.idx];
            _ = state.reader.advance();

            const arr = try state.arena.allocator().alloc(u8, lit.len + 1);

            @memcpy(arr[0..lit.len], lit);

            arr[lit.len] = 0;

            try state.stack.push(.{ .ptr = @ptrCast(@alignCast(arr.ptr)) });
        }
    };
};

// no type checking here because it's only meant to be used internally in one place
fn registerWords(dict: *StringHashMap(Word), comptime args: anytype) !void {
    const args_type_info = @typeInfo(@TypeOf(args));

    const fields = args_type_info.@"struct".fields;

    inline for (fields) |field| {
        const tup = field.defaultValue() orelse @panic("no default value");

        const tup_info = @typeInfo(@TypeOf(tup));

        const source = tup_info.@"struct".fields[0].defaultValue() orelse @panic("no default value");
        const source_info = @typeInfo(source);

        const names = tup_info.@"struct".fields[1].defaultValue() orelse @panic("no default value");

        if (source_info.@"struct".decls.len != names.len)
            @compileError("decls count for " ++ @typeName(source) ++ " does not match count of names");

        inline for (source_info.@"struct".decls, names) |decl, n| {
            const f = @field(source, decl.name);

            const word: Word = if (@typeInfo(@TypeOf(f)) == .@"fn")
                .{ .native = f }
            else
                .{ .composite = f };

            try dict.put(n, word);
        }
    }
}

allocator: Allocator,
arena: std.heap.ArenaAllocator,
err: *[]const u8,
reader: *ZorthReader,
managed_values: Arraylist([]Value),
dict: StringHashMap(Word),
vars: StringHashMap(Value),
stack: ZorthStack,

pub fn init(allocator: Allocator, err: *[]const u8) !Self {
    var z: Self = .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .err = err,
        .reader = try allocator.create(ZorthReader),
        .managed_values = Arraylist([]Value).init(allocator),
        .dict = StringHashMap(Word).init(allocator),
        .vars = StringHashMap(Value).init(allocator),
        .stack = ZorthStack.init(allocator, err),
    };

    z.reader.* = .{};

    try registerWords(&z.dict, .{
        .{ Builtins.Meta, .{ ":", "(", "if" } },
        .{ Builtins.Stack, .{ "dup", "swap", "drop" } },
        .{ Builtins.Math, .{ "+", "-", "*", "/", "%", "pow" } },
        .{ Builtins.IO, .{ ".", "emit", "cr", "print", ".\"", "puts" } },
        .{ Builtins.Ptr, .{ "ref", "reref", "alloc", "c\"" } },
    });

    return z;
}

pub fn deinit(self: *Self) void {
    self.dict.deinit();
    self.stack.deinit();
    _ = self.arena.reset(.free_all);

    if (self.reader.parent != null) {
        self.allocator.destroy(self.reader.parent.?);
    }

    self.allocator.destroy(self.reader);

    for (self.managed_values.items) |ptr| {
        self.allocator.free(ptr);
    }

    self.managed_values.deinit();

    //self.dict.deinit();
    self.vars.deinit();
}

fn exit(self: *Self, code: u8) !void {
    self.err.* = try self.arena.allocator().alloc(u8, 1);
    self.err.*[0] = code;
    return error.Exit;
}

fn errf(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    self.err.* = try self.reader.errf(self.arena.allocator(), fmt, args);
}

fn errs(self: *Self, str: []const u8) !void {
    self.err.* = try self.reader.err(self.arena.allocator(), str);
}

pub fn execute(self: *Self, text: []const u8) !void {
    try self.reader.enter(self.allocator, text);
    defer self.reader.escape(self.allocator);

    while (true) {
        self.reader.skipWhitespace();
        const cur = self.reader.nextSnippet() orelse break;
        //std.debug.print("snip: '{s}'\n", .{cur.text});

        if (std.fmt.parseInt(i32, cur.text, 0) catch null) |int| {
            try self.stack.push(.{ .int = int });
        } else if (std.fmt.parseFloat(f32, cur.text) catch null) |float| {
            try self.stack.push(.{ .float = float });
        } else {
            try self.callFromDict(cur.text);
        }
    }
}

pub fn callWord(self: *Self, word: Word) anyerror!void {
    switch (word) {
        .native => |func| try func(self),
        .composite => |substr| try self.execute(substr),
    }
}

pub fn callFromDict(self: *Self, name: []const u8) !void {
    if (self.dict.get(name)) |word| {
        try self.callWord(word);
    } else {
        try self.errf("unknown word '{s}'", .{name});
        return error.WordNotFound;
    }
}
