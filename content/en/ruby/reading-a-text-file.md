---
title:                "Ruby recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Before we dive into the technical details, let’s talk about why reading a text file is important for any Ruby programmer. Text files are a common way of storing and sharing data, and being able to extract and manipulate information from them is a fundamental skill for data processing and analysis. Whether you're working on a small personal project or a large-scale application, being able to read text files will make your programming tasks much easier.

## How To
Reading a text file in Ruby is a relatively simple process. First, we need to open the file using the `File` class and specify the file mode as `r` for read-only. Next, we use the `read` method to read the contents of the file and store them in a variable. Let's look at an example:

```Ruby
file = File.open("sample.txt", "r")
contents = file.read
puts contents
```

In the above code, we first open the file "sample.txt" in read-only mode and store it in the variable `file`. Then, we use the `read` method to read the contents of the file and store it in the variable `contents`. Finally, we print out the contents using the `puts` method.

We can also specify how many characters we want to read from the file by passing a number as an argument to the `read` method. For example, if we only want to read the first 100 characters of the file, we can use `contents = file.read(100)`.

## Deep Dive
When reading a text file, it's important to understand the different ways in which we can manipulate the file contents. For example, we can use the `each_line` method to iterate through each line of the file and perform specific actions on them. Let's look at an example:

```Ruby
file = File.open("sample.txt", "r")
file.each_line do |line|
  puts line.upcase
end
```

In this code, we use the `each_line` method to iterate through each line of the file and use the `upcase` method to convert the text to uppercase before printing it out.

We can also use the `readlines` method to read all the lines of the file and store them in an array. This allows us to access individual lines and perform operations on them. For example:

```Ruby
file = File.open("sample.txt", "r")
lines = file.readlines
puts "The file has #{lines.length} lines."
```

In this code, we use the `readlines` method to store all the lines of the file in an array called `lines` and then print out the number of lines in the file.

## See Also
For more information on reading and manipulating text files in Ruby, check out the following resources:

- [Ruby's official documentation on the File class](https://ruby-doc.org/core/IO.html)
- [A tutorial on reading and writing files in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [An overview of common file handling operations in Ruby](https://www.digitalocean.com/community/tutorials/how-to-handle-files-in-ruby)

Happy coding!