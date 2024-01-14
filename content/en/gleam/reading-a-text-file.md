---
title:                "Gleam recipe: Reading a text file"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Do you ever find yourself needing to read data from a text file in your programming projects? Maybe you're working with large datasets or parsing configuration files. Whatever the reason may be, understanding how to read a text file in Gleam can greatly benefit your code. In this blog post, we will explore how to do just that.

## How To

To read a text file in Gleam, we will be using the `File` module from the standard library. First, we need to import the module and define the path to our text file.

```
import File

let filepath = "./data.txt"
```

Next, we can use the `File.open` function to open the file and return a `File.FileDescriptor` value. This value represents the open file and allows us to read its contents.

```
let file_descriptor = File.open(filepath)
```

Now that we have our file open, we can read its contents using the `File.read` function. This function takes in the file descriptor and the number of bytes to read as arguments.

```
let num_bytes = 1024
let file_contents = File.read(file_descriptor, num_bytes)
```

Finally, we can close the file using the `File.close` function to free up any resources it may have been using.

```
File.close(file_descriptor)
```

Let's put it all together and see the output of our code.

```
import File

let filepath = "./data.txt"
let file_descriptor = File.open(filepath)
let num_bytes = 1024
let file_contents = File.read(file_descriptor, num_bytes)
File.close(file_descriptor)
```

Output:

```
<<70,105,114,115,116,32,76,105,110...>>
```

The output may look a bit strange, but it is simply a binary representation of the first 1024 bytes of our text file. We can convert it to a string using the `String.from_bytes` function to get a more readable output.

## Deep Dive

Reading a text file using the `File` module may seem simple, but there are a few things to keep in mind. First, when using the `File.read` function, it is important to specify the number of bytes to read. If you do not specify a value, it will try to read the entire contents of the file, which may lead to memory issues for large files.

Secondly, the `File.FileDescriptor` value returned by the `File.open` function can also be used to write to a file using the `File.write` function. This can be helpful when working with configuration files or writing data to a new file.

Additionally, the `File` module offers functions for working with files in different encodings, such as UTF-8 and UTF-16. Make sure to use the appropriate encoding when working with files to avoid unexpected characters or errors.

## See Also

- [Gleam File Module Documentation](https://gleam.run/modules/standard_library/file/)
- [Gleam String Module Documentation](https://gleam.run/modules/standard_library/string/)
- [Gleam Binary Module Documentation](https://gleam.run/modules/standard_library/binary/)