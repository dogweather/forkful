---
title:    "Ruby recipe: Writing a text file"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

In the world of programming, there are many different tasks that may seem simple but require careful attention to detail. One such task is writing a text file. This may not seem like a significant task, but it is a vital aspect of many programs and can greatly enhance the functionality and effectiveness of your code.

## How To

Writing a text file in Ruby is a relatively straightforward process. Let's take a look at a simple example of creating and writing to a text file called "example.txt":

```
Ruby File.open("example.txt", "w") do |file| # Open the file in write mode
  file.puts("This is an example text file.") # Write a string to the file
end # Close the file
```

In this code block, we use the `File.open()` method to open our text file in write mode, denoted by the "w" argument. Next, we use the `.puts()` method to write a string to the file, in this case, "This is an example text file." Finally, we use the `end` keyword to close the file.

If we were to run this code, our "example.txt" file would be created with the specified string as its content. 

## Deep Dive

Now that we have a basic understanding of how to write a text file in Ruby, let's dive a bit deeper into some additional techniques and methods that can enhance our text file writing abilities.

### Writing multiple lines

If we wanted to write multiple lines to our text file, we can use the `.puts()` method multiple times:

```
Ruby File.open("example.txt", "w") do |file|
  file.puts("This is the first line.")
  file.puts("This is the second line.")
  file.puts("This is the third line.")
end
```

This would create a text file with three lines of text, each on a separate line.

### Adding variables

We can also add variables to our text file by using string interpolation within the `.puts()` method:

```
Ruby string = "example"
 
File.open("example.txt", "w") do |file|
  file.puts("This is an #{string} text file.")
end
```

This would result in the string "This is an example text file." being written to our "example.txt" file.

### Appending to an existing file

If we already have a text file and want to add more content to it, we can use the "a" argument instead of "w" in the `File.open()` method. This will open the file in append mode, allowing us to add new content without overwriting the existing content.

## See Also

For more information on working with text files in Ruby, check out these helpful resources:

- [Ruby File class documentation](https://ruby-doc.org/core-2.7.1/File.html)
- [Ruby file I/O tutorial](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [Ruby file methods](https://www.rubyguides.com/2015/02/ruby-file-class/)