---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Cracking the Code: Mastering Temporary Files in Ruby

## What & Why?

Creating a temporary file involves producing a short-lived data storage in your system. Programmers build them to manage data overflow, prevent data loss, or handle large data efficiently during a system's processes.

## How to:

Need to create a temporary file in Ruby? Fear not! Here's how you can do it using the 'tempfile' feature in Ruby's standard library:

```Ruby
require 'tempfile'

temp_file = Tempfile.new('my_temp_file')

temp_file.puts("Hello, temporary file!")
temp_file.rewind 

puts temp_file.read 
# Outputs: "Hello, temporary file!\n"

temp_file.close
temp_file.unlink # this is the same as File.delete(temp_file)
```

This script creates a new temporary file, writes a string in it, rewinds the file pointer to the beginning, then reads and outputs the written string on your screen. Lastly, it closes and deletes the file.

## Deep Dive

The `Tempfile` class in Ruby has been around since its 1.8 version and is designed to make handling temporary files more simple and straightforward. But `Tempfile` is not the only method:

- You could use base `File` class and the `Dir::tmpdir` method to fashion your own solution.
- Another solution would be to use `StringIO`, especially if you merely need a temporary in-memory buffer.

Every temporary file is assigned a unique filename in the OS's temporary directory. The filename's prefix can be customized (as seen in the example: 'my_temp_file'), and the rest of it holds the process id and a sequence number.

## See Also

Creating a temporary file is just a tiny fraction of what Ruby can do. Check out the following links for more Pearls on Programming in Ruby:

- [Ruby Docs: Tempfile](https://ruby-doc.org/stdlib-3.0.0/libdoc/tempfile/rdoc/Tempfile.html)
  
Enjoy coding, and until we meet again!