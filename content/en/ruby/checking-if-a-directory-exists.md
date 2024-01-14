---
title:    "Ruby recipe: Checking if a directory exists"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

When working with files and directories in Ruby, it is important to check if a directory exists before attempting any operations on it. This helps to prevent errors and ensures that your code runs smoothly.

## How To

To check if a directory exists in Ruby, we can make use of the `Dir.exist?()` method. This method takes in the directory path as a parameter and returns a boolean value `true` if the directory exists or `false` if it does not exist.

Let's take a look at an example:

```Ruby
directory = "/Users/johndoe/Documents"
if Dir.exist?(directory)
    puts "Directory exists!"
else
    puts "Directory does not exist."
end
```

In the above code, we first assign the directory path to the `directory` variable. Then, using the `Dir.exist?()` method, we check if the directory exists. If it does, the code inside the `if` statement will be executed, printing out the "Directory exists!" message. Otherwise, the code inside the `else` statement will be executed, printing out "Directory does not exist.".

We can also use string concatenation to make the code more flexible:

```Ruby
directory = "/Users/johndoe"
folder = "Documents"
if Dir.exist?("#{directory}/#{folder}")
    puts "#{folder} directory exists in #{directory}!"
else
    puts "#{folder} directory does not exist in #{directory}."
end
```

In this example, we split the directory path into two separate variables, `directory` and `folder`, and use string interpolation to combine them in the `Dir.exist?()` method.

## Deep Dive

Behind the scenes, the `Dir.exist?()` method is checking if a directory with the given path is a directory object and returning a boolean value accordingly. It uses the `Dir` class which provides a set of methods for working with directories in Ruby.

It is also worth noting that the `Dir.exist?()` method only checks for the existence of the directory itself, not its contents. To check if a directory is empty, we can make use of the `Dir.empty?()` method.

## See Also

- [Dir.exist?() documentation](https://ruby-doc.org/core/Dir.html#method-c-exist-3F)
- [Ruby Directory Class documentation](https://ruby-doc.org/core/Dir.html)
- [Dir.empty?() documentation](https://ruby-doc.org/core/Dir.html#method-c-empty-3F)