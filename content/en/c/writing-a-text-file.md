---
title:    "C recipe: Writing a text file"
keywords: ["C"]
---

{{< edit_this_page >}}

# Why

Writing a text file in a C programming language can be a useful skill to master, especially for developers who work with large amounts of data. It allows for easy organization and storage of information, making it a practical tool for many programming tasks.

# How To

To begin writing a text file in C, we first need to include the necessary header files using the `#include` preprocessor directive. These header files will provide us with the functions and definitions needed for writing and reading files.

```
#include <stdio.h>
#include <stdlib.h>
```

Next, we need to declare a `FILE` pointer variable and use the `fopen` function to open a new file or an existing file for writing. This function takes two arguments: the file name and the mode in which the file should be opened. For writing, we use the mode `"w"`.

```
FILE *file_ptr = fopen("sample.txt", "w");
```

Once the file is open, we can use the `fprintf` function to write data to the file. This function takes three arguments: the file pointer, the format string, and the data to be written.

```
fprintf(file_ptr, "This is an example of writing to a text file using C.");
```

After we are done writing to the file, we need to close it using the `fclose` function. This ensures that all the data is properly saved and the file is ready for use.

```
fclose(file_ptr);
```

Running this code will create a new text file called "sample.txt" in the same directory as the C file.

# Deep Dive

When writing to a text file, it is important to pay attention to the format string used in the `fprintf` function. The format string specifies the type of data being written to the file, and it must match the type of data passed in the third argument.

For example, if we want to write a string to a file, we use the `%s` placeholder in the format string, and pass the string as the third argument.

```
fprintf(file_ptr, "%s", "This is an example of a string being written to a text file.");
```

We can also use other placeholders such as `%d` for integers, `%f` for floating-point numbers, and `%c` for characters.

Another important aspect to keep in mind is error handling. It is possible for the file to not open successfully or for the `fprintf` function to fail. It is good practice to check for errors by using the `ferror` function, and handle them accordingly to avoid any unexpected behavior.

# See Also

- [C File Input and Output](https://www.programiz.com/c-programming/c-file-input-output)
- [C File Handling Tutorial](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C Standard Library - File Input/Output Functions](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)