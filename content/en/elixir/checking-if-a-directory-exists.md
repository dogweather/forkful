---
title:                "Checking if a directory exists"
date:                  2024-01-19
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Elixir ensures that you're interacting with a valid file path. Programmers do it to avoid errors like trying to read from or write to a non-existent location, which could crash their app or break a process.

## How to:

Elixir's `File` module is your go-to for directory checks. Use `File.dir?/1` to return a boolean indicating if the directory exists.

```elixir
# Check if directory exists
if File.dir?("/path/to/directory") do
  IO.puts("Directory exists!")
else
  IO.puts("No such directory.")
end
```

Sample output for an existing directory:
```elixir
Directory exists!
```

Sample output for a non-existing directory:
```elixir
No such directory.
```

## Deep Dive

Historically, file system operations have carried significance in programming owing to the need for reading/writing data. In Elixir, the `File` module has abstracted these operations neatly. It's all about reliability with these checks; thus, `File.dir?/1` is a staple for verifying paths.

Alternatives to `File.dir?/1` can be using `File.stat/2` and checking if the result is `:ok`, which indicates the directory exists. Another approach might be utilizing `:filelib.is_dir/1` from Erlang's standard library, which Elixir can access due to its interoperability with Erlang.

Elixir's implementation of checking if a directory exists wraps around Erlang's robust file handling. This design leverages the BEAM's capability for fault-tolerant systems, wherein Elixir applications typically run.

## See Also

- Elixir's `File` module documentation: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Erlang's `filelib` module documentation for more file system functions: [http://erlang.org/doc/man/filelib.html](http://erlang.org/doc/man/filelib.html)
- Robust file handling in Elixir: [https://elixir-lang.org/getting-started/io-and-the-file-system.html](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
