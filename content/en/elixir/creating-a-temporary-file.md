---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file involves generating a file that is only needed for a limited period, typically for an ongoing process or session. Programmers create these files to provision writable space for temporary data, preserve system memory, and prevent potential data losses during a recalculation or rerunning process.

## How to:

In Elixir, you can create a temporary file using Erlang's `:file` module. Here's how to create such a file and write content to it:

```elixir
{:ok, file} = :file.mktemp() # Creates a temporary file
:file.write(file, "Temporary file content") # Writes to the file
```

And you can read content from the file like this:

```elixir
{:ok, content} = :file.read_file(file) # Reads content from the file
IO.puts(content) # Outputs the content
```

This will output:

```elixir
"Temporary file content"
```

## Deep Dive

Historically, temporary files provide an efficient way to store auxiliary data that doesn't need to permanently exist. They were especially important in older systems with limited memory resources. 

Elixir doesn't have a native method for generating temporary files, thus we use Erlang's `:file.mktemp()`. However, there are alternative approaches. For example, you might manually create a new file in the system's temporary directory.

Implementation-wise, `:file.mktemp()` creates the file with a unique name in the temporary directory (usually `/tmp` on Unix or Linux systems). The default permissions set the file to read-write for the current user, and no permissions for others.

Lastly, remember to delete the file when you're done. You can do this with `:file.delete(file)`.

## See Also

For more details, refer to Erlang's documentation on the `:file` module. 

1. ["Erlang's :file module documentation"](http://erlang.org/doc/man/file.html)
2. ["How to create a temporary file in Elixir?" StackOverflow Discussion ](https://stackoverflow.com/questions/43367559/how-to-create-a-temporary-file-in-elixir)
3. ["Working with files and IO"](https://elixir-lang.org/getting-started/io-and-the-file-system.html) in Elixir's Getting Started guide.