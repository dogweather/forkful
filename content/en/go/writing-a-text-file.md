---
title:    "Go recipe: Writing a text file"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Many programming languages, including Go, allow users to create and manipulate text files. These files can contain various types of information, such as text, numbers, and symbols. In Go, writing a text file can be a useful skill for managing data and creating reports or documents.

## How To

To start writing a text file in Go, we first need to import the `os` and `fmt` packages. The `os` package allows us to create, open, and manipulate files, while the `fmt` package allows us to format and print data.

Next, we need to use the `os.Create()` function to create a new file. This function takes in the file name as a string and returns a `File` object, which we can use to write to the file.

```Go
file, err := os.Create("my_file.txt")
if err != nil {
    fmt.Println(err)
}
```

After creating the file, we can use the `File` object's `WriteString()` method to write data to the file. This method takes in a string as a parameter, and we can use the `fmt.Sprintf()` function to format our data into a string.

```Go
data := fmt.Sprintf("This is some data to be written to the file!")
_, err = file.WriteString(data)
if err != nil {
    fmt.Println(err)
}
```

Once we have finished writing to the file, we need to close it using the `Close()` method to ensure that all data is saved properly.

```Go
err = file.Close()
if err != nil {
    fmt.Println(err)
}
```

To check if our file has been created and written to successfully, we can use the `cat` command in our terminal to view the contents of the file.

```
cat my_file.txt
```

The output should be: `This is some data to be written to the file!`.

## Deep Dive

Now that we have learned the basics of writing a text file in Go, let's take a deeper look at some additional features and considerations.

### Writing Multiple Lines

To write multiple lines to a text file, we can use the `WriteString()` method multiple times, each time with a different string. Alternatively, we can use the `fprintf()` or `fmt.Fprintf()` functions to write formatted data directly to the file.

```Go
data := fmt.Sprintf("First line \n")
data += fmt.Sprintf("Second line")
_, err = file.WriteString(data)
if err != nil {
    fmt.Println(err)
}
```

### Appending to an Existing File

To append data to an existing file, we can use the `os.OpenFile()` function and pass in the file name and the `os.O_APPEND` flag. This will allow us to open an existing file for writing while preserving its contents.

```Go
existingFile, err := os.OpenFile("my_file.txt", os.O_APPEND, 0644)
if err != nil {
    fmt.Println(err)
}
```

We can then use the same methods mentioned above (`WriteString()`, `fprintf()`, `fmt.Fprintf()`) to append data to the file.

### Error Handling

When writing to a file, it is important to handle errors properly. In our examples, we use the `if err != nil` condition to check for and handle any errors that may occur. It is also recommended to use the `defer` statement when handling files to ensure that the file is always closed, even in the case of an error.

```Go
defer file.Close()
```

## See Also

- [Go File I/O](https://golang.org/pkg/os/#Create)
- [Go Formatting with fmt](https://golang.org/pkg/fmt/)