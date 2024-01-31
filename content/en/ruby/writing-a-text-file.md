---
title:                "Writing a text file"
date:                  2024-01-19
simple_title:         "Writing a text file"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to a text file in Ruby means saving data to a file on your system. Programmers do it for data persistence, logging, and data sharing between different programs or program runs.

## How to:

To write to a text file in Ruby, use the `File` class. Here's a quick example:

```Ruby
File.open("output.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end
```

Sample output (contents of `output.txt`):
```
Hello, Ruby!
```

To append to an existing file, use the "a" mode:

```Ruby
File.open("output.txt", "a") do |file|
  file.puts "Appending this line."
end
```

Output (additional contents of `output.txt`):
```
Appending this line.
```

## Deep Dive

Ruby's file handling has its roots in UNIX file I/O operations. The `open` method can take a block, automatically closing the file afterward, which is unique and convenient compared to some other languages. Alternatives to `File.open` include `IO.write` for quick writes and different libraries like `CSV` or `FileUtils` for specialized tasks.

When you're writing to a file, be mindful of character encoding and line endings especially when your file needs to be read by different systems or languages.

## See Also

- Ruby's IO class: https://ruby-doc.org/core/IO.html
- Ruby's FileUtils: https://ruby-doc.org/stdlib/libdoc/fileutils/rdoc/FileUtils.html
- Ruby-Doc File class: https://ruby-doc.org/core/File.html
