---
title:                "Gleam recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
When working with data in programming, it is often necessary to read information from a text file. This can be useful for importing data, manipulating it, and then exporting it back into a file. In this blog post, we will explore how to use Gleam to efficiently read a text file and extract the information we need.

## How To
To read a text file in Gleam, we first need to import the standard library's `file` module. We can then use the `read_file` function to read the contents of a file into a string.

```Gleam
import file

let contents = file.read_file("my_file.txt")
```

We can also specify the encoding of the file using the optional `encoding` argument. By default, `read_file` uses UTF-8 encoding.

Once we have the contents of the file, we can use Gleam's `strings` module to split the string into lines. This will give us a list containing each line of the file as a separate string.

```Gleam
import file
import strings

let contents = file.read_file("my_file.txt")
let lines = strings.lines(contents)
```

We can then loop through the lines and perform any necessary manipulation or extraction of data.

```Gleam
for line in lines {
  // Do something with the line
}
```

To output the results, we can use the `io` module's `println` function.

```Gleam
import io

io.println("Line: " ++ line)
```

## Deep Dive
When reading a text file, it's important to understand the potential issues that may arise. One common issue is handling different line endings, such as `\n` (Unix) or `\r\n` (Windows). Gleam's `file` and `strings` modules handle this automatically, allowing you to work with the lines regardless of the line ending used in the file.

Another consideration is memory usage, especially when working with large files. Gleam's `file` module uses lazy evaluation, which means lines are only read from the file as needed. This can help prevent memory overflow when dealing with large files.

## See Also
- Gleam documentation for [`file`](https://gleam.run/documentation/standard-library/file)
- Gleam documentation for [`strings`](https://gleam.run/documentation/standard-library/strings)
- Gleam documentation for [`io`](https://gleam.run/documentation/standard-library/io)