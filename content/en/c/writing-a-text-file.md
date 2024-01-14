---
title:    "C recipe: Writing a text file"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are essential for storing data and information in a human-readable format. They are commonly used in programming as a way to save and access data, making them an essential tool for developers. Whether you are creating a simple text-based game or writing a complex database application, understanding how to write a text file in C is an important skill to have.

## How To

To write a text file in C, we need to use the `fopen()` function to open a file in "write" mode. This function takes two arguments: the name of the file we want to create or overwrite, and the mode in which we want to open the file. In our case, we will use the mode "w" to open the file in write mode. 

```C
FILE *fp;
fp = fopen("example.txt", "w");
```

After opening the file, we can use the `fprintf()` function to write data to the file. This function works similarly to the `printf()` function, except it takes an additional argument that specifies the file to write to. Here's an example of how we can write a string to our file:

```C
fprintf(fp, "Hello world!");
```

Once we have finished writing to the file, we need to close it using the `fclose()` function. This will ensure that all the data is properly written to the file.

```C
fclose(fp);
```

Now, if we open our file, we should see the string "Hello world!" written in it. However, if we want to write multiple lines of text, we need to use the newline character `\n` to separate each line. Here's an example:

```C
fprintf(fp, "This is line 1\n");
fprintf(fp, "This is line 2\n");
fprintf(fp, "This is line 3");
```

The output of this code in our file would look like this:

```
This is line 1
This is line 2
This is line 3
```

## Deep Dive

In C, writing a text file is essentially a matter of writing a stream of characters to a file. The `fprintf()` function allows us to specify the format of these characters using format specifiers, just like we would with `printf()`. For example, `%d` is used to print an integer, `%f` for a floating-point number, and `%s` for a string.

In addition to "write" mode, there are two other modes that we can use to open a file. "Read" mode (`"r"`) allows us to read data from a file and "append" mode (`"a"`) allows us to add data to the end of a file without overwriting the existing data. It's important to note that if a file does not exist, using "read" or "append" mode will result in an error.

## See Also

- [Writing Files in C](https://www.programiz.com/c-programming/c-file-input-output)
- [C File Handling](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C File I/O Cheat Sheet](https://www.cs.cmu.edu/~ab/15-123S15/resources/C/cheat-sheets.html#fileio)