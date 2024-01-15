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

## Why 

Creating a temporary file in Go can be useful for a variety of reasons. Temporary files are often used in programs that require temporary storage space for processing large amounts of data, or for creating backups and logs.

## How To

To create a temporary file in Go, we can use the `ioutil` package. Below is a simple example that creates a temporary file named "temp.txt" in the current directory.

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Create temporary file
    tempFile, err := ioutil.TempFile(".", "temp.txt") // "." represents current directory
    
    if err != nil {
        panic(err)
    }
    
    // Write data to temporary file
    _, err = tempFile.Write([]byte("This is a temporary file created in Go!"))
    
    if err != nil {
        panic(err)
    }
    
    // Close temporary file
    err = tempFile.Close()
    
    if err != nil {
        panic(err)
    }
    
    // Print temporary file name
    fmt.Println("Created temporary file:", tempFile.Name())
}
```

Running this code will create a temporary file named "temp.txt" in the current directory and write the specified data to it. The `TempFile` function takes the first parameter as the directory to create the file in, and the second parameter as the prefix for the temporary file name. The returned `*os.File` can be used to write data to the file, and must be closed using the `Close` method to avoid any potential file corruption.

The output of the above code will look like this:

```
Created temporary file: ./temp.txt
```

## Deep Dive 

The `ioutil.TempFile` function internally calls the `TempDir` function from the same package to generate a unique temporary directory. This directory is typically located in the system's default temporary directory, and the temporary file is created within it.

Additionally, the `ioutil` package also provides the `TempDir` and `TempFile` functions separately, in case you need more control over where your temporary file is created.

## See Also

- [ioutil documentation](https://golang.org/pkg/io/ioutil/)
- [tempfile package](https://golang.org/pkg/tempfile/)
- [creating temporary files in Go](https://www.callicoder.com/go-create-temporary-file/)