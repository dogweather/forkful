---
date: 2024-02-03 19:03:14.348240-07:00
description: "How to: Ruby makes file operations straightforward. To write to a file,\
  \ you can use Ruby's built-in `File` class. The following example demonstrates how\u2026"
lastmod: '2024-03-13T22:45:00.568026-06:00'
model: gpt-4-0125-preview
summary: Ruby makes file operations straightforward.
title: Writing a text file
weight: 24
---

## How to:
Ruby makes file operations straightforward. To write to a file, you can use Ruby's built-in `File` class. The following example demonstrates how to open a file for writing (`"w"` mode) and append (`"a"` mode), then write a string to it, and ensure the file is closed afterwards:

```ruby
# Writing new content to a file, overwriting existing content
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# Appending content to the end of a file
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
After running both snippets, the content of `example.txt` will be:
```
Hello, Ruby!
Adding another line.
```

### Using a third-party library: FileUtils
For more complex file operations, the Ruby standard library `FileUtils` can come in handy, though for basic file writing, standard `File` methods are sufficient. However, if you want to copy, move, remove, or perform other filesystem operations in conjunction with file writing, `FileUtils` is worth exploring.

An example of using `FileUtils` for creating a directory and then writing to a file within that directory:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

This demonstrates creating a new directory `logs` if it doesn't already exist, and writing to a new file `today.log` within it, showcasing both directory and file manipulation without directly writing with FileUtils, but utilizing its directory handling capability.
