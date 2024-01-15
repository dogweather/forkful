---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a common task in programming, and must be done correctly in order to access and manipulate data. This article will guide you through the process of reading a text file in Go, ensuring that you have the necessary skills to tackle this task in your own projects.

## How To

Reading a text file in Go is a simple process that involves a few steps. First, we need to open the file using the `os.Open` function.

```Go
file, err := os.Open("data.txt")
if err != nil {
  // handle error
}
```

Next, we need to read the contents of the file using the `bufio` package. This package provides a `Scanner` type that makes it easy to read a file line by line.

```Go
scanner := bufio.NewScanner(file)
for scanner.Scan() {
  line := scanner.Text()
  // do something with the line
}
```

Finally, don't forget to close the file after you have finished reading it.

```Go
err = file.Close()
if err != nil {
  // handle error
}
```

Let's put it all together in a working example. Assume we have a file named "data.txt" with the following contents:

```
1,John,Doe
2,Jane,Smith
3,Bob,Johnson
```

Our goal is to read this file and print out each line. Here is the complete code:

```Go
package main

import (
  "bufio"
  "fmt"
  "os"
  "strings"
)

func main() {
  file, err := os.Open("data.txt")
  if err != nil {
    panic(err)
  }
  defer file.Close()

  scanner := bufio.NewScanner(file)
  for scanner.Scan() {
    line := scanner.Text()
    fmt.Println(strings.Split(line, ","))
  }
}
```

The output of this program will be:

```
[1 John Doe]
[2 Jane Smith]
[3 Bob Johnson]
```

## Deep Dive

Now let's take a deeper look at what is happening in our code. When we use `os.Open` to open a file, we are provided with a `File` type, which represents an open file descriptor. This type has a `Read` method that we can use to read data from the file.

However, the `bufio.Scanner` type abstracts away the lower-level details of reading data from a file, making it much easier for us to work with. It handles the reading and buffering of data, and provides a convenient `Text` method to retrieve the current line being read.

Additionally, the `strings.Split` function allows us to split a string based on a delimiter, in this case a comma. This is useful for parsing the data from our file.

## See Also

- [Go documentation on files](https://golang.org/pkg/os/#File)
- [Go documentation on bufio package](https://golang.org/pkg/bufio/)
- [Go documentation on strings package](https://golang.org/pkg/strings/)