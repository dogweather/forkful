---
title:                "Checking if a directory exists"
html_title:           "Ruby recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists is a common task in programming that involves verifying the existence of a specific folder or directory in the file system. Programmers often do this to ensure that their code can successfully access and work with the required files or directories.

## How to:
To check if a directory exists in Ruby, we can use the ```Dir.exist?``` method. This method takes in a directory path as an argument and returns a boolean value indicating whether the directory exists or not.

```ruby
# Example directory path
directory_path = "/home/username/Documents"

# Check if directory exists
if Dir.exist?(directory_path)
  puts "The directory exists."
else
  puts "The directory does not exist."
end
```

The output of the above code will be:

```
The directory exists.
```

If the directory path specified does not exist, the output will be:

```
The directory does not exist.
```

## Deep Dive:
In previous versions of Ruby, the ```Dir.exists?``` method was used to check if a directory exists. However, this method has been deprecated in favor of using ```Dir.exist?``` for consistency with the ```File.exist?``` method.

Alternatively, we can also use the ```File.directory?``` method to check if a given path leads to a directory. This method returns true if the path leads to a directory, and false if it leads to a file.

```ruby
# Example directory path
directory_path = "/home/username/Documents"

# Check if directory exists
if File.directory?(directory_path)
  puts "The path leads to a directory."
else
  puts "The path leads to a file."
end
```

The output of the above code will be:

```
The path leads to a directory.
```

## See Also:
- [Dir.exist? documentation](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F)
- [File.exist? documentation](https://ruby-doc.org/core-3.0.0/File.html#method-c-exist-3F)
- [File.directory? documentation](https://ruby-doc.org/core-3.0.0/File.html#method-c-directory-3F)