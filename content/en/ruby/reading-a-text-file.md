---
title:    "Ruby recipe: Reading a text file"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Are you a Ruby programmer looking to expand your skills? Do you need to work with text files in your code? If so, then this blog post is for you! In this post, we will delve into the world of reading text files in Ruby, giving you the tools you need to successfully incorporate this functionality into your projects.

## How To

To read a text file in Ruby, we will be using the `File` class and its `open` method. Let's take a look at a basic example:

```ruby
# First, we'll want to define the path to our text file
filepath = "sample.txt"

# Next, we use the open method to open the file
File.open(filepath, "r") do |file|

  # Inside the open block, we can now access the file's contents
  contents = file.read
  # For demonstration purposes, we'll print the contents to the console
  puts contents
end
```

In this example, we create a `filepath` variable and set it to the path of our text file. Then, using the `File.open` method with the "r" flag (indicating that we want to read the file), we open the file and assign it to the `file` variable within the block. Using the `file.read` method, we can access and read the file's contents, and in this case, we simply print it to the console. Of course, the possibilities are endless when it comes to what you can do with the file's contents - you can manipulate the data, save it to a new file, and so on.

## Deep Dive

It's important to note that the `File.open` method has many different flags you can use, depending on your needs. For example, if you want to write to the file, you would use the "w" flag instead of "r". Additionally, you can specify the encoding of the file if needed by adding `:encoding => 'utf-8'` as an argument in the `open` method. And if you want to read and write to the same file, you can use the "r+" flag. For more information on these flags and other methods available in the `File` class, be sure to check out the Ruby documentation.

## See Also

If you found this blog post helpful, be sure to check out these other great resources on working with text files in Ruby:

- [Official Ruby Documentation on File class](https://ruby-doc.org/core-2.7.2/File.html)
- [RubyMonk's article on Working with Files in Ruby](https://rubymonk.com/learning/books/5-ruby-primer-ascent/chapters/34-file-i-o/lessons/81-conclusion)