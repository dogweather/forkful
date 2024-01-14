---
title:    "Go recipe: Creating a temporary file"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Creating temporary files in Go can be a useful tool for developers when working with large datasets or when needing to store temporary data that is not needed for long periods of time. Temporary files can also be helpful for debugging purposes, as they can be easily deleted once the debugging process is complete. 

## How To

To create a temporary file in Go, we will be using the `ioutil` package. This package provides various functions for IO operations, including creating and manipulating temporary files. Let's take a look at an example:

```Go
package main

import (
  	"fmt"
  	"io/ioutil"
)

func main() {
  	// Create a temporary file with prefix "temp-" and in the default temporary directory.
  	tempFile, err := ioutil.TempFile("", "temp-")
  	if err != nil {
    		fmt.Println(err)
  	}
    
  	// Close and remove the temporary file once we are done with it.
  	defer func() {
    		tempFile.Close()
    		err = os.Remove(tempFile.Name())
    		if err != nil {
      			fmt.Println(err)
    		}
  	}()
    
  	// Write data to the temporary file.
  	data := []byte("Hello, world!")
  	_, err = tempFile.Write(data)
  	if err != nil {
    		fmt.Println(err)
  	}
  	
  	// Read the temporary file and print the output.
  	output, err := ioutil.ReadFile(tempFile.Name())
  	if err != nil {
    		fmt.Println(err)
  	}
  	fmt.Println(string(output))
}
```

Running this code will create a temporary file with a name that begins with "temp-" in the default temporary directory. We use the `ioutil.TempFile` function to create the file, passing in an empty string for the directory and "temp-" as the prefix. This function returns a file pointer and an error, so we use those to check if everything was successful. 

Next, we use `defer` to close the file and remove it once we are done with it. This ensures that the temporary file is deleted automatically, even if there is an error during the execution of our code. 

We then write some data, in this case "Hello, world!", to the file using the `Write` function. And finally, we read the file and print the output. 

## Deep Dive

Behind the scenes, the `ioutil.TempFile` function is using the `os.Create` function, which in turn uses the `mkstemp` function from the C standard library. This function creates a file with a unique name in the specified directory, and returns a file descriptor and the name of the file. The `ioutil.TempFile` function then uses that descriptor to create a `*os.File` and returns it to us.

It is important to note that the temporary file created using `ioutil.TempFile` will have `0600` permissions by default, meaning it can only be read and written to by the owner. If you need to change the permissions, you can do so after creating the file using the `os.Chmod` function.

## See Also

- [ioutil package documentation](https://golang.org/pkg/io/ioutil/)
- [os package documentation](https://golang.org/pkg/os/)
- [Creating temporary files in Python](https://realpython.com/python-tempfile/)