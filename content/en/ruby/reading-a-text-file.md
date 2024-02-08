---
title:                "Reading a text file"
aliases:
- en/ruby/reading-a-text-file.md
date:                  2024-01-20T17:54:51.421325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file means accessing the content of a file stored on disk through code. Programmers do it to process, analyze, or display data within their applications.

## How to:

Reading a file in Ruby is straightforward. You can use the `File` class, which provides different methods to read files. Here's a simple example of reading an entire file:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

If `example.txt` contains the text "Hello, Ruby!", here's what you'll get:

```
Hello, Ruby!
```

For reading line by line:

```Ruby
File.foreach("example.txt") { |line| puts line }
```

Same `example.txt`, now output will be line by line:

```
Hello, Ruby!
```

## Deep Dive:

Historically, reading files has been a core feature of programming languages, allowing interactions with the filesystem.

In Ruby, you can also read a file with different tools:

1. `IO` class: For low-level file operations.
2. `readlines` method: Loads the entire file into an array, with each line as an element.
3. `File.read`: Quick way to read an entire file into a string.

There's a trade-off to consider: `File.read` is neat for small files, but it can be memory intensive for larger ones. That's when reading line by line or in chunks becomes valuable.

## See Also:

- Ruby Docs for the `File` class: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Stack Overflow discussions on file reading in Ruby: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)
