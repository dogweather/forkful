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

## Why

If you're a Ruby programmer, you know the importance of error handling and ensuring your code can handle unexpected situations. One common scenario is needing to check if a directory exists before performing operations on it, such as creating a new file inside it. This prevents errors and improves the overall reliability and stability of your code.

## How To

To check if a directory exists in Ruby, you can use the `Dir.exist?` method. Let's take a look at an example:

```ruby
# Check if a 'documents' directory exists
if Dir.exist?('documents')
  puts "The 'documents' directory already exists!"
else
  puts "Creating the 'documents' directory..."
  Dir.mkdir('documents')
end
```

In the example above, we first use the `exist?` method to check if the 'documents' directory exists. If it does, we print a message stating that it already exists. If it doesn't, we use the `mkdir` method to create the directory.

You can also use the `Dir.exist?` method in combination with the `File.directory?` method to check if a specific path is a directory, like this:

```ruby
# Check if a specific path is a directory
path = 'users/john/documents'
if Dir.exist?(path) && File.directory?(path)
  puts "'#{path}' is a directory!"
end
```

As you can see, we first use the `Dir.exist?` method to check if the path exists, then we use the `File.directory?` method to confirm that it is a directory.

## Deep Dive

Under the hood, the `Dir.exist?` method uses the `Dir.open` method to open the specified path and check if it is a directory. It then returns a boolean value based on the result. The `File.directory?` method also uses a similar approach, but it also checks for additional details such as permissions and file type.

It's worth mentioning that both these methods will return `true` even if the specified path is a symbolic link to a directory. If you want to specifically check if the actual directory exists, you can use the `File.realdirpath` method.

## See Also

- [Dir.exist? documentation](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F)
- [File.directory? documentation](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- [File.realdirpath documentation](https://ruby-doc.org/core-2.7.1/File.html#method-c-realdirpath)