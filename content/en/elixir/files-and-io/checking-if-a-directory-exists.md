---
title:                "Checking if a directory exists"
aliases:
- /en/elixir/checking-if-a-directory-exists.md
date:                  2024-02-03T19:02:24.890926-07:00
model:                 gpt-4-0125-preview
simple_title:         "Checking if a directory exists"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists in Elixir is about verifying the presence of a directory at a specified path in the file system. Programmers do this to ensure they can safely read from, write to, or perform operations on the directory without encountering errors due to its absence.

## How to:
Elixir's standard library offers a straightforward way to check for the existence of a directory through the `File` module. Here's how you can use it:

```elixir
if File.dir?("path/to/directory") do
  IO.puts "Directory exists!"
else
  IO.puts "Directory does not exist."
end
```

Sample output, assuming the directory does not exist:
```
Directory does not exist.
```

For more advanced filesystem interactions, including checking directory existence, you might consider using third-party libraries like `FileSystem`. While Elixir's standard capabilities are sufficient for many cases, `FileSystem` can offer more nuanced control and feedback for complex applications. However, for the basic need of checking if a directory exists, sticking to the native `File` module is typically recommended since it's readily available and doesn't require any external dependencies.
