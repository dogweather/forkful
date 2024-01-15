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

## Why

Writing a text file using Ruby is a useful skill to have, especially for those who are working on projects that require reading and writing data. It allows for easy manipulation and storage of information without relying on a database.

## How To

To create a text file using Ruby, we first need to create a new file and open it for writing, specifying the file name and the "w" mode for writing. Then, we can use the `.write` method to add text to the file. Finally, we need to close the file to ensure all changes are saved.

```
```Ruby
file = File.open("example.txt", "w")
file.write("This is an example text file created using Ruby! \n")
file.write("It's as easy as using the .write method! \n")
file.close
```
```

Running this code will create a new file called "example.txt" with the specified text inside. The `\n` is used to indicate a line break in the file.

We can also use string interpolation to add variables or dynamically generated content to our text file. For example:

```
```Ruby
name = "John"
age = 30

file = File.open("example2.txt", "w")
file.write("#{name} is #{age} years old. \n")
file.close
```
```

This will create a new file called "example2.txt" with the text "John is 30 years old" inside.

## Deep Dive

When writing a text file, it's important to keep in mind the mode we use for opening the file. As mentioned, we used "w" for writing, but there are other modes we can use such as "a" for appending to an existing file or "r+" for both reading and writing.

Additionally, we can add more advanced options such as specifying the encoding of the file or using the `puts` method instead of `.write` to automatically add a line break after each piece of text.

There are also different ways to format the text in our file, such as using `.printf` or `.sprintf` methods to add formatted data or using the `<<` operator to concatenate strings.

## See Also

- [Ruby File Class](https://ruby-doc.org/core-2.7.2/File.html)
- [Ruby String Class](https://ruby-doc.org/core-2.7.2/String.html)
- [RubyIO - The Basics](https://www.rubyguides.com/2018/11/ruby-io/)

By practicing writing text files in Ruby, you'll have a useful skill that can be applied to various projects and tasks. Keep exploring the different methods and options available to hone your skills and make use of this powerful feature in Ruby. Happy coding!