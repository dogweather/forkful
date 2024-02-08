---
title:                "Writing a text file"
aliases:
- en/ruby/writing-a-text-file.md
date:                  2024-02-03T19:03:14.348240-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing a text file"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to a text file in Ruby is a fundamental operation that allows you to store output and data persistently, enabling data to be accessed or modified later. Programmers often perform this task for reasons such as logging, saving configurations, or exporting data in a human-readable format.

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
