---
title:                "Reading a text file"
html_title:           "Ruby recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file in Ruby refers to the process of accessing and manipulating the content of a plain text file using Ruby code. This can be useful for tasks such as extracting data, parsing information, or performing text-based operations. Programmers often read text files in order to automate repetitive tasks or to handle large amounts of data in a more efficient and organized manner.

## How to:

To read a text file in Ruby, we can use the File class and its methods. Let's say we have a file called "data.txt" in our current directory, containing the following text:

```
Hello world!
This is a sample file.
```

We can use the `open` method to open the file and access its contents:

```Ruby
file = File.open("data.txt")

# read the entire file
puts file.read #=> Hello world!\nThis is a sample file.

# read the first line
puts file.readline #=> Hello world!

# read all lines as an array
puts file.readlines #=> ["Hello world!\n", "This is a sample file."]

# close the file
file.close
```

We can also use a block to automatically close the file once we are done with it:

```Ruby
File.open("data.txt") do |file|
  # read the entire file
  puts file.read #=> Hello world!\nThis is a sample file.
end
```

## Deep Dive

Reading text files has been a fundamental part of programming since the early days of computing. In Ruby, the `File` class was introduced in version 1.9.3 and has since been the go-to method for reading and manipulating text files.

Aside from using methods such as `read`, `readline` and `readlines`, we can also use the `foreach` method to iterate over each line in a file:

```Ruby
File.foreach("data.txt") do |line|
  # print each line
  puts line #=> Hello world!\nThis is a sample file.
end
```

There are other methods that can be used to read from a file, such as `gets` and `getc`, but these are not commonly used for reading text files.

## See Also

Check out the official Ruby documentation on the `File` class for more information and examples on reading text files: https://ruby-doc.org/core-2.6.1/File.html

To learn more about manipulating text files in Ruby, you can also refer to the "Ruby File Handling" guide on tutorialspoint: https://www.tutorialspoint.com/ruby/ruby_file_handling.htm