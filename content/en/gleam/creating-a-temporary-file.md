---
title:                "Gleam recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming, especially in situations where the program needs to temporarily store data or files before proceeding with further operations. Temporary files are also used when working with external libraries or APIs that require temporary file paths as inputs. In short, temporary files are essential for efficient and seamless execution of code.

## How To

To create a temporary file in Gleam, we use the `file.temp` function from the `gleam/io` module. This function takes in a file extension as an argument and returns a tuple containing the temporary file path and a file handle.

```
Gleam.def
  create_temp_file() {
    let {_, file} = Gleam.IO.File.temp(".txt")

    // perform operations on the temporary file using the `file` handle

    Gleam.IO.File.close(file) // close the file when done
  }
```

The temporary file path in the tuple can be used to read, write or manipulate the temporary file, while the file handle ensures that the temporary file is closed correctly after usage.

To get the path of the temporary file, we access it from the tuple using the `Tuple.first` function. Similarly, the file handle can be accessed using `Tuple.second` function. Here's an example of reading from a temporary file:

```
Gleam.def
  read_temp_file() {
    let {path, file} = Gleam.IO.File.temp(".csv")
    let data = Gleam.IO.File.read(file)

    // perform operations on the `data` variable which contains the contents of the temporary file

    Gleam.IO.File.close(file) // close the file when done
  }
```

## Deep Dive

Creating a temporary file in Gleam is a two-step process. First, the `file.temp` function creates a file with a unique name and returns a file handle. Then, the `file` handle is used to perform desired operations on the temporary file. The file is automatically deleted when the handle is closed, or the program exits.

It's worth noting that the naming of temporary files is taken care of by the operating system. Therefore, temporary file names may vary and may not be consistent across operating systems. Additionally, temporary files should only be used for short-term storage and should not be assumed to persist between program executions.

## See Also

- Gleam documentation on temporary files: <insert link>
- How to use temporary files in other programming languages: <insert links>