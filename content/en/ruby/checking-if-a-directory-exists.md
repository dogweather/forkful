---
title:                "Ruby recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
Checking if a directory exists is an important aspect of programming, especially when dealing with file management tasks. It allows us to ensure that our code is robust and prevents errors that may occur when attempting to access non-existent directories.

## How To
To check if a directory exists in Ruby, we can use the `Dir.exist?()` method. This method takes in a string representing the path of the directory and returns a boolean value. Let's take a look at an example:

```Ruby
if Dir.exist?("blog_posts")
  puts "The 'blog_posts' directory exists."
else
  puts "The 'blog_posts' directory does not exist."
end
```

The above code checks if a directory named "blog_posts" exists in the current directory. If it does, it will print a message stating that it exists, and if not, it will print a message stating that it does not exist.

We can also use the `File.directory?()` method to check if a directory exists. This method works similarly to the `Dir.exist?()` method, except it takes in a file or directory path as an argument. Let's see it in action:

```Ruby
if File.directory?("/Users/username/Documents")
  puts "The 'Documents' directory exists."
else
  puts "The 'Documents' directory does not exist."
end
```

If the "Documents" directory exists in the specified file path, the code will output a message saying so. Otherwise, it will output a message stating that it does not exist.

## Deep Dive
Under the hood, both the `Dir.exist?()` and `File.directory?()` methods use the `FileTest.exist?()` method. This method checks if the specified path exists, whether it is a file or directory.

It is worth noting that the `Dir.exist?()` and `File.directory?()` methods only check for the existence of directories and not the validity. This means that they will return `true` even if the specified path leads to a file or a broken symlink.

## See Also
- [Ruby Documentation on File and Directory Classes](https://ruby-doc.org/core-2.7.1/File.html)
- [Check if Directory Exists in Ruby](https://www.rubyguides.com/2017/09/ruby-directory-exists/)
- [Working with Files and Directories in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)