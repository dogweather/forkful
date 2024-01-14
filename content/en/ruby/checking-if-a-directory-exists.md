---
title:                "Ruby recipe: Checking if a directory exists"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

At some point in your coding journey with Ruby, you may come across the need to check if a directory exists. This could be for various reasons, such as ensuring that a specific directory is present before running a certain process, or for error handling purposes. Regardless of the reason, it is a useful skill to have in your toolbox.

## How To

To check if a directory exists in Ruby, we can use the `Dir.exist?()` method. This method takes in a directory path as its argument and returns a boolean value of `true` if the directory exists, and `false` if it doesn't.

Let's take a look at an example:

```
```Ruby
# Check if the directory "documents" exists
Dir.exist?("documents")

# Output: false
```

```

In the above code, we are using the `Dir.exist?()` method to check if the "documents" directory exists. Since it doesn't, the method returns `false` as the output.

Now, let's say we have a directory called "pictures" and we want to check if it exists before performing a certain action:

```
```Ruby
# Check if the directory "pictures" exists
Dir.exist?("pictures")

# Output: true

# Perform some action if the directory exists
if Dir.exist?("pictures")
  puts "Directory found. Proceeding with action."
  # Code to perform action goes here
else
  puts "Directory not found. Please create the directory and try again."
end
```

In this example, we use an `if` statement to check if the "pictures" directory exists. If it does, we execute the code inside the `if` block, and if it doesn't, we execute the code inside the `else` block.

## Deep Dive

Behind the scenes, the `Dir.exist?()` method uses the `File.directory?()` method to determine if a given path points to a directory or not. Therefore, these two methods can be used interchangeably when checking if a directory exists.

It is also worth noting that the `Dir.exist?()` method only checks for the existence of the directory, not its accessibility or permissions. So, even if the directory exists, if you don't have the proper permissions to access it, the method will still return `false`.

## See Also

- [Ruby Documentation on `Dir.exist?()`](https://ruby-doc.org/core-2.7.0/Dir.html#method-c-exists-3F)
- [Ruby Documentation on `File.directory?()`](https://ruby-doc.org/core-2.7.0/File.html#method-c-directory-3F)
- [Tutorial on checking if a directory exists in Ruby](https://www.rubyguides.com/2018/10/ruby-file-directory-exists/)

Happy coding!