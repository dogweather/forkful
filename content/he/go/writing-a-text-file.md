---
title:                "כתיבת קובץ טקסט"
html_title:           "Go: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 

So you want to learn how to write a text file in Go? Well, the process is quite simple and can come in handy when you need to store data in a simple and readable format. Plus, learning how to write a text file is a great skill to have as a Go programmer.

## How To 

To write a text file in Go, we will be using the built-in `os` and `io/ioutil` packages. Let's start by creating a new file called `myFile.txt` using the `os.OpenFile()` function. We will also pass in the `os.O_CREATE` and `os.O_WRONLY` flags as arguments to specify that we want to create a new file and write to it respectively. 

```Go 
file, err := os.OpenFile("myFile.txt", os.O_CREATE|os.O_WRONLY, 0644) 
if err != nil { 
    log.Fatal(err) 
} 
```

Next, we will use the `WriteString()` function from the `io/ioutil` package to write our desired text to the file. 

```Go 
text := "Hello, world!" 
err = ioutil.WriteString(file, text) 
if err != nil { 
    log.Fatal(err) 
} 
```

And voila, we have written our text file! Don't forget to close the file using the `Close()` function to ensure all changes are saved. 

```Go 
err = file.Close() 
if err != nil { 
    log.Fatal(err) 
} 
``` 

You can also use the `bufio.NewWriter()` function for more efficient writing of large text files. Just remember to call the `Flush()` function to ensure all data is written to the file. 

## Deep Dive 

You may have noticed that we specified the permissions for our file using `0644`. This is a Unix file mode that specifies read and write permissions for the owner, and only read permissions for group and other users. The `O_CREATE` and `O_WRONLY` flags can also be combined with other flags such as `O_TRUNC` to truncate the file if it already exists.

Additionally, you can use the `os.OpenFile()` function to open an existing file for writing by passing in the `os.O_APPEND` flag. This will append any new text to the end of the file instead of overwriting it.

## See Also 

- Official Go Documentation on writing files: https://golang.org/pkg/os/#OpenFile
- Detailed tutorial on writing text files in Go: https://tutorialedge.net/golang/reading-writing-files-in-go/