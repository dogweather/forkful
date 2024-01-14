---
title:    "Gleam recipe: Writing a text file"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file may seem like a simple task, but as a programmer, it is important to understand its significance. Text files are essential for storing data in a format that is easily readable and shareable between different programs. They are also the basis for creating more complex file types, such as CSV or JSON, which are used extensively in web development. In this blog post, we will explore the basics of writing a text file in Gleam programming language.

## How To
To start, we need to create a new project and import the standard io module. Then, we can use the function `io.file` to create a new file. Here's an example that creates a file named "my_file.txt" and writes "Hello World!" to it:

```Gleam
import io

fn write_to_file() {
  let file = io.file("my_file.txt")
  file.write("Hello World!")
  file.close()
}
```
After we have written our content to the file, it is important to remember to close it to save the changes.

We can also append text to an existing file by using the `append` function instead of `write`. Here's an example:

```Gleam
import io

fn append_to_file() {
  let file = io.file("my_file.txt")
  file.append("This is a new line!")
  file.close()
}
```

## Deep Dive
Now that we have seen how to create and write to a text file in Gleam, let's delve deeper into some important concepts.

When creating a file, it is always a good practice to check for errors and handle them accordingly. Gleam provides an `Error` type that can be used for this purpose. Here's an example that handles any errors that may occur while creating a file:

```Gleam
import io

fn create_file() {
  let result = io.file("my_file.txt")
  case result {
    Err(error) -> error.reason
    Ok(file) -> {
       file.write("File created successfully!")
       file.close()
    }
  }
}
```

Another important concept to keep in mind is file encoding. By default, Gleam uses UTF-8 encoding for text files. However, if you need to use a different encoding, you can specify it as a second argument in the `file` function.

## See Also
- [Gleam documentation on io module](https://gleam.run/documentation/stdlib/io)
- [Tutorial on creating and writing to files in Gleam](https://gleam.run/tutorials/file-io)

Writing a text file may seem like a simple task, but understanding the basics and important concepts will make you a more proficient programmer. Happy coding!