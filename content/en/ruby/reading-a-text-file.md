---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file involves scanning a file on the disk to retrieve its contents and bring it into your program. Programmers do this to manipulate data for various uses like analysis, transformation, feeding into another system, etc.

## How to:

Reading a text file in Ruby is straightforward. The `File` class in Ruby's standard library helps us do just that.

```Ruby
# Read an entire text file 
entire_file = File.read('myfile.txt')
puts entire_file
```

This will output the entire contents of "myfile.txt" as a single string. If you're looking to read line by line:

```Ruby
# Read an entire text file line by line
File.foreach('myfile.txt') do |line|
  puts line
end
```

The `foreach` method takes a block of code to execute for each line in the file.

## Deep Dive:

Historically, reading files was a low-level task involving system calls and plenty of room for error. Modern languages like Ruby provide higher-level, safer and friendlier ways to read files.

There are several other ways to skin this cat. For instance, we can use `File.open` method to open a file and read its contents. However, `File.read` and `File.foreach` are widely recommended due to their simplicity.

On OS-level implementation, Ruby internally uses buffered I/O operations. It reads a chunk of data into a buffer, and then serves future read requests from this buffer. This is significantly faster than making a system call for each read operation.

## See Also:

Great articles to further your reading:

- Ruby file I/O: [link](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- Buffered and unbuffered I/O: [link](https://www.ionos.com/digitalguide/server/know-how/buffered-vs-unbuffered-io/)
- API documentation for Ruby File class: [link](https://ruby-doc.org/core-3.0.0/File.html)