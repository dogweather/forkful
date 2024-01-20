---
title:                "Writing a text file"
html_title:           "Ruby recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file in Ruby means creating a file on your computer that contains text. Programmers do this to store data in a more permanent and accessible way, or to save the results of a program for later use.

## How to:

To write a text file in Ruby, you can use the File class and its corresponding methods. First, you'll need to open or create the file using the `File.open()` method, and specify the file name and the writing mode (`w`). Then, you can use the `puts` method to add the desired text to your file. Finally, use the `close` method to save your changes and close the file.

```Ruby
# Create a new text file and write text to it
File.open("my_file.txt", "w") do |file|
  file.puts "This is some text written to a file!"
end

# Open an existing text file and append text to it
File.open("my_file.txt", "a") do |file|
  file.puts "This text will be added to the end of the file."
end
```

This will result in a `my_file.txt` file that contains the following text:

```
This is some text written to a file!
This text will be added to the end of the file.
```

## Deep Dive:

Writing text files has been a fundamental task in programming for a long time. Text files can contain different types of data, making them versatile for storing information in a readable format. Alternatives to writing text files in Ruby include using other languages like Python or even using a database. However, text files offer a simple and accessible way to save data without needing any external tools or software.

When writing a text file in Ruby, it's essential to pay attention to the writing mode used. There are three main modes: `w` (write mode), `a` (append mode), and `r` (read mode). Using `puts` in `w` mode will overwrite the entire existing file, while `puts` in `a` mode will add the text to the end of the file without deleting its content. Also, don't forget to close the file after making changes to save your changes and release the resources used to access the file.

## See Also:

- [Ruby File Class Documentation](https://ruby-doc.org/core-3.0.0/File.html)