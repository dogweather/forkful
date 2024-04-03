---
date: 2024-02-03 19:02:24.890926-07:00
description: "How to: Elixir's standard library offers a straightforward way to check\
  \ for the existence of a directory through the `File` module. Here's how you can\
  \ use\u2026"
lastmod: '2024-03-13T22:44:59.795379-06:00'
model: gpt-4-0125-preview
summary: Elixir's standard library offers a straightforward way to check for the existence
  of a directory through the `File` module.
title: Checking if a directory exists
weight: 20
---

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
