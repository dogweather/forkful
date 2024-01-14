---
title:                "Ruby recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to save information in a structured and easily readable manner? Look no further than writing a text file with Ruby! It's a quick and simple way to store data that can be accessed and manipulated later on.

## How To

To start, create a new Ruby file and require the 'File' module. Then, use the 'File.new' method to create a new text file. You can name the file whatever you'd like and even specify the file path if you want to save it in a specific location.

```Ruby
require 'File'

File.new("example.txt", "w")
```

Next, we can use the 'File.open' method to open the text file we just created and write to it. The first parameter is the name of the file, while the second parameter specifies the mode in which the file will be opened. In this case, we use "a" to append to the end of the file.

```Ruby
File.open("example.txt", "a") do |file|
  file.puts "Hello, world!"
end
```

Now, if we open the text file, we will see the line "Hello, world!" written in it. We can also use variables to add more dynamic data to the text file.

```Ruby
name = "John"
age = 25

File.open("example.txt", "a") do |file|
  file.puts "#{name} is #{age} years old."
end
```

This will add the line "John is 25 years old." to the text file.

## Deep Dive

When writing a text file with Ruby, there are a few important things to keep in mind. Firstly, writing to a file will overwrite any existing data, unless you use "a" to append to the end. Additionally, the 'puts' method will automatically add a new line to the end of each entry, while the 'print' method will not.

If you need to read from a text file, you can use the 'File.foreach' method to iterate through each line in the file. You can also use the 'File.read' method to read the entire contents of the file as a string.

Lastly, it is important to always close the file after you are done writing to it using the 'File.close' method. This will save any changes you have made and prevent any unexpected behavior.

## See Also

- [Ruby File class](https://ruby-doc.org/core-3.0.1/File.html)
- [Ruby FileUtils module](https://ruby-doc.org/stdlib-3.0.1/libdoc/fileutils/rdoc/FileUtils.html)
- [Learn Ruby - Text Files](https://learnruby.com/textfiles.html)