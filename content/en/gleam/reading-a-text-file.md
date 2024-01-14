---
title:    "Gleam recipe: Reading a text file"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

If you're new to Gleam programming, you may be wondering why it's necessary to learn how to read text files. Text files are a crucial part of any programming language as they allow you to store and retrieve data in a human-readable format. Without the ability to read text files, it would be challenging to work with external data in your Gleam programs.

## How To

Reading a text file in Gleam is a simple process. You first need to define a file path, which is the location of the text file on your computer. Once you have the file path, you can use the `File.read_text` function to read the contents of the file and store it in a variable. Let's take a look at an example:

```
Gleam will open the file at the given path, and read it's   contents into a string

let file_path = "my_file.txt"
let contents = File.read_text(file_path)
```

In this example, we have defined the file path as `my_file.txt` and assigned the contents of the file to the `contents` variable. We can then use this variable in our program to work with the data from the text file. 

## Deep Dive

When reading a text file, it's essential to understand the encoding of the file. An encoding is a set of rules that determine how characters are stored and represented in a file. If the encoding is not specified, Gleam will default to UTF-8 encoding. However, if your text file uses a different encoding, you can specify it when reading the file by providing a second argument to the `File.read_text` function. 

In some cases, you may also encounter errors when trying to read a text file. These errors can be caused by various factors, such as incorrect file paths or file permissions. It's crucial to understand these error messages and troubleshoot them appropriately to ensure successful reading of text files in your programs.

## See Also

Here are some helpful resources for further reading on reading text files in Gleam:

- [Gleam Language Documentation](https://gleam.run/book/tour/reading-files.html)
- [Gleam File Library](https://gleam.run/std/file.html)
- [Unicode and Character Encodings](https://www.w3.org/International/questions/qa-choosing-encodings)
 
Happy coding!