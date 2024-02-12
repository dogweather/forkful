---
title:                "Створення тимчасового файлу"
aliases:
- uk/ruby/creating-a-temporary-file.md
date:                  2024-01-20T17:41:11.103703-07:00
model:                 gpt-4-1106-preview
simple_title:         "Створення тимчасового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?

Temporary files are files created to hold data temporarily while a program is running. Programmers use them for tasks like managing large data that won't fit into memory, inter-process communication, and storing data that doesn't need to persist after the program ends.

## How to: / Як це зробити:

Ruby provides a simple way to work with temporary files using the `Tempfile` class. Here's an example:

```Ruby
require 'tempfile'

Tempfile.create('tempfile_example') do |tempfile|
  # Write something to the temporary file
  tempfile.write("Hello, this is a temporary message!")

  # Rewind the file before you read it, just like a cassette tape
  tempfile.rewind

  # Read from the file
  puts tempfile.read  # Output: Hello, this is a temporary message!

  # No need to delete, it's handled automatically
end
```

## Deep Dive / Поглиблене вивчення:

The `Tempfile` class has been a part of Ruby's Standard Library for ages, helping developers manage temporary files easily. An alternative is to manage files manually, creating and deleting them, which is riskier due to potential file leaks. Under the hood, `Tempfile` creates files in a special temp directory, which on most UNIX-like systems is '/tmp'. The files are usually named with a random sequence to avoid clashes, and get deleted automatically when the object is garbage collected or when the program exits.

## See Also / Дивіться також:

- Ruby's Standard Library documentation: [https://ruby-doc.org/stdlib/](https://ruby-doc.org/stdlib/)
- For an in-depth look at file I/O: [https://www.tutorialspoint.com/ruby/ruby_input_output.htm](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
