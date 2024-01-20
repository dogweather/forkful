---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists in Ruby means determining if a certain folder exists in the file system. As programmers, we do this to avoid errors that occur when trying to access or modify a non-existent directory.

## How to:

In Ruby, you can use the `Dir.exist?` method from `Dir` class to check if a directory exists. Here's an example:

```Ruby
if Dir.exist?('/path/to/directory')
  puts 'Directory exists!'
else
  puts 'Directory does not exist!'
end
```

This script will output "Directory exists!" if the path provided after `Dir.exist?` points to a directory that exists, or "Directory does not exist!" if it doesn't.

## Deep Dive

Historically, the `File.directory?` method was used to check if a directory exists in Ruby. However, `Dir.exist?` is now preferred because it makes the code's intention clear.

An alternate way of checking directory existence is by using the `::new` method, which will raise an `Errno::ENOENT` exception if the directory does not exist. 

```Ruby
begin
  Dir::new('/path/to/directory')
  puts 'Directory exists!'
rescue Errno::ENOENT
  puts 'Directory does not exist!'
end
```

Another thing to note is that checking directory existence is not atomic in nature. That means another process might delete the directory after you've checked it but before you start using it. You should handle the `Errno::ENOENT` exception whenever you try to access a directory.

## See Also

- Ruby's [Dir class Documentation](https://ruby-doc.org/core-2.7.0/Dir.html)
- Stackoverflow Post: ["How to check if a directory exists in Ruby"](https://stackoverflow.com/questions/753918/how-to-check-if-a-directory-exists-in-ruby)