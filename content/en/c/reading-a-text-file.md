---
title:                "Reading a text file"
html_title:           "C recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is an essential skill for any programmer working with data or file management. Text files are a common way to store and handle structured information, and being able to read them is necessary for tasks such as input validation and data processing.

## How To

Reading a text file in C involves a few simple steps. First, we need to open the file using the `fopen()` function, which takes in the file name and the mode in which we want to open the file (e.g. read, write, append). 

```C
FILE *file = fopen("example.txt", "r"); // opens the file in read mode
```

Next, we need to check if the file was opened successfully before proceeding to read it. We can do this by checking if the `fopen()` function returns a non-null value.

```C
if (file == NULL) {
    printf("Error opening file!\n");
    exit(1);
}
```

Once the file is successfully opened, we can use the `fgets()` function to read each line of the file. This function takes in a buffer to store the read line, the maximum number of characters to read, and the file pointer.

```C
char buffer[256]; // buffer to store the read line
while(fgets(buffer, 256, file) != NULL) { // reads line by line until the end of the file
    printf("%s", buffer); // prints the read line
}
```

Finally, we need to close the file using the `fclose()` function to free up any resources allocated to it.

```C
fclose(file);
```

**Sample Output:**

```
Hello world!
This is a text file.
It contains information.
```

## Deep Dive

There are a few things to consider when reading a text file in C. 

Firstly, the `fgets()` function reads the newline character at the end of each line, so we need to remove it from the buffer before processing the data. This can be achieved by using the `strcspn()` function, which returns the number of characters in the given string until the newline character is encountered.

```C
buffer[strcspn(buffer, "\n")] = '\0'; // removes the newline character from the buffer
```

Additionally, we can also specify the character encoding when opening the text file. This is important when reading files that contain non-English characters. The default character encoding in C is ASCII, but we can use the `fopen()` function with the "rb" mode to specify the encoding as binary and read the file correctly.

```C
FILE *file = fopen("example.txt", "rb"); // opens the file in binary mode for reading
```

Lastly, we can also use the `fscanf()` function to read specific data from the file instead of reading line by line. This function works similarly to `scanf()` in taking in a format specifier, but it also takes in the file pointer as an additional argument.

```C
int num;
while(fscanf(file, "%d", &num) == 1) { // reads integers until the end of the file
    printf("%d ", num);
}
```

## See Also

For more information on file handling in C, check out the following resources:

- [File Handling in C - GeeksforGeeks](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Reading and Writing Files in C - Programiz](https://www.programiz.com/c-programming/c-file-input-output)
- [File Input/Output in C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)