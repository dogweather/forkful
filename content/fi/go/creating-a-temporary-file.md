---
title:    "Go: Väliaikaistiedoston luominen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary files, or temporary stores, are important for many reasons in programming. They can be used for storing data that needs to be accessed for a short period of time, or for testing out code without affecting existing files. In Go, creating a temporary file is a straightforward process that can greatly improve the efficiency and organization of your code.

## Kuinka Tehdä

Creating a temporary file in Go involves using the 'ioutil' package and its 'TempFile' function. First, we import the package:

```Go
import "io/ioutil"
```

Next, we use the 'TempFile' function, specifying the directory where we want the temporary file to be created and the prefix for the file name. The function will return a pointer to the temporary file and an error, which we can handle with a simple if statement:

```Go
tempFile, err := ioutil.TempFile("/tmp", "testfile_")
if err != nil {
    // handle error
}
```

We can then write data to the temporary file using the 'WriteString' method:

```Go
tempFile.WriteString("This is a temporary file.")
```

And finally, we close the file, which will delete it from the specified directory:

```Go
tempFile.Close()
```

The above code will create a temporary file called "testfile_xxxx" in the /tmp directory, where 'xxxx' is a random string of characters. It will then write the given string to the file and close it, effectively deleting it from the directory. This process provides an easy and safe way to create and use temporary files in Go.

## Syvällisempi Sukellus

The 'TempFile' function in the 'ioutil' package creates a file with default permissions and opens it for reading and writing. If you need to change these default settings, you can use the 'TempFileIn' function instead. This function allows you to specify additional flags, such as file mode and permissions, when creating the temporary file. It also returns a similar file object and error, which can be handled in the same way.

Additionally, you can use the 'TempDir' function to create a temporary directory instead of a file. This function takes in the same parameters as 'TempFile', but returns a string representing the path to the temporary directory. You can then use this directory for storing and accessing multiple temporary files.

## Katso Myös

- [ioutil package documentation](https://golang.org/pkg/io/ioutil/)
- [tempfile function documentation](https://golang.org/pkg/io/ioutil/#TempFile)
- [tempdir function documentation](https://golang.org/pkg/io/ioutil/#TempDir)