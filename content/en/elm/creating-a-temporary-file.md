---
title:                "Elm recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common task in programming. It allows for the temporary storage and manipulation of data before it is permanently saved or discarded. In Elm, temporary files can be useful for tasks like file uploads, caching, and data processing.

## How To

To create a temporary file in Elm, we first need to import the `File` module:

```Elm
import File
```

Next, we can use the `create` function from the `File` module to create a temporary file. This function takes in two arguments: a filename and the content of the file.

```Elm
File.create "temp.txt" "This is the content of the temporary file." 
```

This will create a temporary file named `temp.txt` with the given content. We can also use `File.create` to create a file with data from a specific source, such as a URL or a user's input:

```Elm
File.create "temp.txt" model.fileData
```

Once the temporary file is created, we can manipulate and use it as needed within our program. It is important to remember that temporary files are not automatically deleted, so we need to use the `delete` function from the `File` module to remove them when they are no longer needed.

## Deep Dive

Behind the scenes, creating a temporary file in Elm actually creates a real file on your computer's file system. However, the file's name will have a unique indicator attached to it, such as a random number or timestamp, to differentiate it from other files.

It is also worth noting that the temporary file will be stored in a system-specific temporary directory, rather than the directory where your Elm code is located. This is to prevent cluttering your code directory with temporary files.

When using `File.create`, it is also possible to specify the type of data you are including in the file by providing a third argument, such as `text/plain` or `image/png`. This can be useful for file uploads or when working with different types of data.

## See Also

- [Elm File module documentation](https://package.elm-lang.org/packages/elm/file/latest/)
- [Temporary files in programming](https://en.wikipedia.org/wiki/Temporary_file)