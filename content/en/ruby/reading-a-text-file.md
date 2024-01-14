---
title:                "Ruby recipe: Reading a text file"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading and processing text files is a common task in programming, and it's no different in Ruby. Being able to read text files allows us to access and manipulate large amounts of data, making our programs more dynamic and useful.

## How To
To begin, we need to open the text file in our Ruby program. We can use the built-in File class and its `open` method to accomplish this:

```ruby
my_file = File.open("sample.txt", "r") # r stands for "read" mode
```

We use the `r` mode to open the file in read-only mode. Next, we can use the `read` method to read the contents of the file:

```ruby
content = my_file.read
```

This will store the contents of the file in the `content` variable. We can then print the contents to the console or manipulate it in any way we like.

To read from a specific line in the file, we can use the `readlines` method and specify the line number:

```ruby
lines = my_file.readlines
second_line = lines[1] # index starts at 0
```

We can also use the `each` method to iterate through the lines of the file one by one:

```ruby
my_file.each do |line|
  # do something with each line
end
```

Once we're done using the file, it's important to close it using the `close` method:

```ruby
my_file.close
```

Failure to close the file can lead to issues and errors, so make sure to always include this step.

## Deep Dive
When reading a text file, we can specify the encoding to ensure that the file is read correctly. We can do this by passing an additional argument to the `open` method:

```ruby
my_file = File.open("sample.txt", "r", encoding: "UTF-8")
```

This will ensure that the file is read using the UTF-8 encoding, which is a popular and widely-used standard for text files.

We can also use the `gets` method to read one line of the file at a time, rather than loading the entire file into memory. This can be useful for large files that we don't want to load all at once.

Lastly, it's important to handle any potential errors when reading a text file. We can use the `begin` and `rescue` keywords to handle errors and exceptions that may arise.

## See Also
- [Ruby File class documentation](https://ruby-doc.org/core-2.7.0/File.html)
- [Reading and Writing Text Files in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/) by RubyGuides
- [Ruby File Handling Tutorial](https://www.techotopia.com/index.php/Ruby_File_Handling) by Techotopia