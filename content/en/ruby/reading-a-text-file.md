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

## Why

Reading a text file is a common task in programming, especially when dealing with data processing or file manipulation. In this article, we will explore how to read a text file using Ruby, the popular and user-friendly programming language.

## How To

To read a text file in Ruby, we will use a combination of built-in methods and File.open(). Let's say we have a text file called "data.txt" which contains the following content:

```
Hello
World
```

To read this file, we can use the following code:

```
File.open("data.txt").each do |line|
	puts line
end
```

The ```open``` method opens the file in read-only mode, while the ```each``` method iterates through each line in the file. In this case, it will print out:

```
Hello
World
```

We can also specify a variable to store the content of each line, like this:

```
File.open("data.txt").each do |line|
	content = line.chomp
	puts content
end
```

The ```chomp``` method removes any trailing newline characters from the line, ensuring that our output doesn't have any extra line breaks.

We can also specify a block of code to perform for each line, like this:

```
File.open("data.txt").each do |line|
	puts "The current line is: " + line
end
```

This will output:

```
The current line is: Hello
The current line is: World
```

We can also read the entire text file as a single string using the ```read``` method, like this:

```
content = File.read("data.txt")
puts content
```

This will output:

```
Hello
World
```

## Deep Dive

In Ruby, we can also specify the mode in which we want to open the file, using the ```File.open()``` method. For example, if we want to open the file in write-only mode, we can use the mode "w" like this:

```
File.open("data.txt", "w").write("This is a new line.")
```

This will overwrite the existing content of the file with the new string "This is a new line.". We can also use the mode "a" to append content to the end of the file, like this:

```
File.open("data.txt", "a").write("This is another new line.")
```

This will add the string "This is another new line." to the end of the file, without overwriting the existing content.

Additionally, we can specify the encoding of the file when opening it, using the optional second argument in the ```File.open()``` method. For example, if our text file is in UTF-8 encoding, we can open it like this:

```
File.open("data.txt", "r:UTF-8").each do |line|
	puts line
end
```

This ensures that any special characters or symbols in the file are properly read and displayed.

## See Also

For more information on File I/O in Ruby, check out the following links:

- [Official Ruby documentation on File class](https://ruby-doc.org/core-2.7.1/File.html)
- [Ruby File Handling Tutorial](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Reading and Writing Files in Ruby](https://www.geeksforgeeks.org/reading-and-writing-files-in-ruby/)