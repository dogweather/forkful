---
title:                "Checking if a directory exists"
date:                  2024-01-20T14:58:17.837383-07:00
html_title:           "Gleam recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists means confirming whether a folder is present in the file system. Programmers do it to avoid errors before attempting file operations like reading from or writing to a directory.

## How to:

In Ruby, you can use the `Dir.exist?` method to check if a directory exists. Here's how it looks:

```ruby
if Dir.exist?("/path/to/directory")
  puts "Directory exists!"
else
  puts "No such directory."
end
```

Sample output when the directory exists:

```
Directory exists!
```

And when it doesn't:

```
No such directory.
```

## Deep Dive

Historically, Ruby has offered multiple ways to interact with the file system. `Dir.exist?` is now a preferred method due to its clarity and simplicity, but older code might use `File.directory?`. Both methods mean virtually the same thing and are more or less interchangeable.

```ruby
# Using File.directory? to achieve the same result
if File.directory?("/path/to/directory")
  puts "Directory exists!"
else
  puts "No such directory."
end
```

Why the redundancy? It's a part of Ruby's principle of giving programmers more than one way to do something. Yet, `Dir.exist?` can be considered a more semantically accurate way to check for directories specifically.

In terms of under-the-hood implementation, when you invoke `Dir.exist?`, Ruby asks the operating system to check the file system, which checks if the specified path points to a directory.

When it comes to alternatives, besides manual path checking, you could also catch the exceptions that result from trying to access a non-existent directory. However, this isn't recommended because it's more expensive in terms of system resources and less clear to someone reading the code.

## See Also

To delve further into Ruby’s file and directory handling, check out the following resources:

- Ruby Docs on `Dir` class: [https://ruby-doc.org/core/Dir.html](https://ruby-doc.org/core/Dir.html)
- Ruby Docs on `File` class: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- For good coding practices on error handling: [https://www.honeybadger.io/blog/ruby-exception-vs-standarderror-whats-the-difference/](https://www.honeybadger.io/blog/ruby-exception-vs-standarderror-whats-the-difference/)