---
title:    "Elixir recipe: Checking if a directory exists"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

If you're an Elixir programmer, chances are you've encountered the need to check if a directory exists at some point in your coding journey. Whether it's for error handling, file manipulation, or other reasons, knowing how to check if a directory exists can save you time and prevent bugs in your code.

## How To

The good news is that Elixir provides an easy and straightforward way to check if a directory exists. All you need is the `File.exists?/1` function, which takes in a path to the directory as its argument.

Let's take a look at an example using a directory called "my_folder":

```Elixir
File.exists?("my_folder")
```

The output of this code would be a boolean value, either `true` or `false`, indicating whether the directory exists or not.

If you want to handle the case where the directory does not exist, you can use the `if` statement along with the `File.exists?/1` function. Here's an example:

```Elixir
if File.exists?("my_folder") do
  # do something if directory exists
else
  # do something if directory does not exist
end
```

As you can see, checking if a directory exists using Elixir is a simple and intuitive process.

## Deep Dive

Behind the scenes, the `File.exists?/1` function uses the `Path.expand/1` function to expand any relative paths to absolute paths before checking if the file or directory exists. This ensures that the correct path is being checked and prevents potential errors.

It's also worth noting that the `File.exists?/1` function can be used to check for the existence of both directories and files. So you can use it in your code to handle different scenarios accordingly.

## See Also

Now that you know how to check if a directory exists in Elixir, here are a few resources to further enhance your understanding and improve your Elixir skills:

- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Path module documentation](https://hexdocs.pm/elixir/Path.html)
- [Elixir if statement documentation](https://elixir-lang.org/getting-started/case-cond-and-if.html#if)

Happy coding!