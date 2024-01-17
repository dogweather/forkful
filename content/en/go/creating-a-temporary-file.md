---
title:                "Creating a temporary file"
html_title:           "Go recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file is the process of generating and storing data in a temporary location on a computer's storage device. Programmers use this technique to store data that is only needed temporarily, such as holding intermediate results or storing user preferences during a session.

## How to:
Here's how you can create a temporary file in Go:

```
Go package main

import (
    "io/ioutil"
    "os"
)

func main() {
    // create a new temporary file
    tempFile, err := ioutil.TempFile("", "example")

    // handle error if any
    if err != nil {
        panic(err)
    }

    // write data to temporary file
    data := []byte("Hello, world!")
    _, err = tempFile.Write(data)

    // handle error if any
    if err != nil {
        panic(err)
    }

    // print temporary file name
    fmt.Println("Temporary file name:", tempFile.Name())

    // close temporary file
    tempFile.Close()

    // remove temporary file
    os.Remove(tempFile.Name())

}
```

Sample output:
```
Temporary file name: /var/tmp/example055949 
```

## Deep Dive:
Creating temporary files has been a common practice in programming since the early days of computing when computers had limited memory and storage space. This technique allowed for efficient use of resources by storing data temporarily and removing it once it was no longer needed.

In Go, there are multiple options for creating temporary files, including using the `ioutil.TempFile()` function as shown in the previous example or using the `os.CreateTemp()` function. These functions handle all the necessary steps for creating a temporary file, including generating a unique name and setting the appropriate permissions.

Alternatively, programmers can also manually create a temporary file by specifying a file path and using the `os.Create()` function. However, doing so requires extra steps, such as handling file permission and deleting the file afterwards.

## See Also:
- [Go Documentation on creating temporary files](https://golang.org/pkg/io/ioutil/#TempFile)
- [Alternatives to creating temporary files in Go](https://medium.com/web-engineering-co/alternatives-to-temporary-files-in-go-1cab7662174f)
- [Implementation details of temporary files in Go](https://medium.com/web-engineering-co/implementation-of-go-ioutil-tempfile-part-1-402e39f90a5b)