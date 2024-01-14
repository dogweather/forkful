---
title:                "Elixir recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're new to Elixir programming, you might be wondering why anyone would want to write a text file. Well, there are many reasons to do so - from storing configuration settings to creating log files for debugging purposes. Plus, understanding how to write a text file is a handy skill to have in your programming toolkit.

## How To

Writing a text file in Elixir is a straightforward process. First, we need to define the name and path of our file using the `File.open/2` function. Then, we can use the `IO.write/2` function to write our desired text into the file. Let's see an example:

```Elixir
file = File.open("example.txt", [:write])
IO.write(file, "This is a sample text.")
File.close(file)
```

In the code above, we first open a file named "example.txt" with the `File.open` function. The second argument, `:write`, specifies that we want to write to the file. Next, we use the `IO.write` function to actually write the text we want into the file. Finally, we close the file with the `File.close` function.

Now, if we open the "example.txt" file, we should see the text "This is a sample text." written inside. Easy, right?

## Deep Dive

If you're interested in learning more about writing text files in Elixir, it's important to understand that there are various options and functions available to customize your file writing experience. For example, you can use `:append` instead of `:write` in the `File.open` function to append text to a file that already exists. Additionally, the `IO.write` function has options for specifying character encoding and line endings.

Furthermore, it's important to remember to properly handle errors when writing a text file. This can be done using the `File.write/3` function, which returns `{:ok, bytes_written}` if the file was successfully written, or `{:error, reason}` if there was an error.

## See Also

To learn more about writing text files in Elixir, check out the following resources:

- [Elixir File Module Documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir IO Module Documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir File and Path Manipulation - A Quick Guide](https://medium.com/@brucepomeroy/elixir-file-and-path-manipulation-a-quick-guide-f48d36c94d4c)

Happy coding!