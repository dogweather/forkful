---
title:                "Ruby recipe: Writing a text file"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file may seem like a basic task for most programmers, but it plays a crucial role in many programming projects. Text files allow us to store and retrieve data easily, making them an essential part of the development process. Whether you are creating a simple text-based application or building a complex program, understanding how to write a text file in Ruby is a valuable skill to have.

## How To

Writing a text file in Ruby is a simple process, but it requires a few steps. Let's break down the process into more manageable chunks.

First, we need to open a file and specify the mode in which we want to write to it. The `File.open()` method allows us to do this. We can specify the mode as `w` to indicate that we want to write to the file. For example:

```Ruby
file = File.open("new_file.txt", "w")
```

Next, we need to use the `write()` method to enter the text we want to save in the file. For example, if we want to write the string "Hello World" in our file, we can use the following code:

```Ruby
file.write("Hello World")
```

Finally, we need to close the file using the `close()` method to save the changes made to the file. Our complete code would look like this:

```Ruby
file = File.open("new_file.txt", "w")
file.write("Hello World")
file.close()
```

When we run this code, it will create a new file named "new_file.txt" in the same directory as our Ruby file. The file will contain the text "Hello World."

## Deep Dive

In addition to the `write()` method, Ruby also has other methods that allow us to manipulate text files. For example, we can use the `puts()` method to write a line of text in the file and automatically add a line break.

```Ruby
file = File.open("new_file.txt", "w")
file.puts("Hello")
file.puts("World")
file.close()
```

This code will create a file with two lines of text: "Hello" and "World."

We can also use the `puts()` method in combination with loops to write multiple lines of text in a file. For example, if we have an array of names and we want to write each name on a separate line in a file, we can use the following code:

```Ruby
names = ["John", "Mary", "Mark"]

file = File.open("names.txt", "w")
names.each do |name|
  file.puts(name)
end
file.close()
```

The resulting file will contain:

```
John
Mary
Mark
```

## See Also
- Ruby File Class - [https://ruby-doc.org/core-2.7.0/File.html](https://ruby-doc.org/core-2.7.0/File.html)
- Ruby IO Class - [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)
- Ruby File Manipulation Tutorial - [https://www.rubyguides.com/2015/05/working-with-files-ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby)

Writing a text file in Ruby may seem straightforward, but there are many ways to manipulate text files and save time and effort in the development process. With a good understanding of the File and IO classes in Ruby, you can easily create, modify, and save text files to suit your project's needs. Happy coding!