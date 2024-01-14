---
title:                "C recipe: Reading a text file"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Text files are a common and important form of data storage in programming. They allow for easy access and manipulation of large amounts of data. By learning how to read a text file, you can enhance your programming skills and make your code more efficient.

## How To
To read a text file in C, we first need to open the file using the `fopen()` function. This function takes two parameters - the file name and the mode in which it will be opened (read, write, append etc.). For example:

```C
FILE *fp;
fp = fopen("data.txt", "r");
```

Next, we need to read the contents of the file using the `fscanf()` function. This function takes three parameters - the file pointer, the format specifier and the variable to store the data in. For example, if our file contains a string, we can use `fscanf()` as follows:

```C
char str[50];
fscanf(fp, "%s", str);
```

We can also read the contents of the file line by line using the `fgets()` function. This function takes three parameters - the buffer to store the data in, the maximum number of characters to read and the file pointer. For example, to read a line of text from our file, we can use `fgets()` as follows:

```C
char line[100];
fgets(line, 100, fp);
```

After we have finished reading the file, we need to close it using the `fclose()` function. This is important as it frees up any resources used by the file. For example:

```C
fclose(fp);
```

Now let's put it all together and look at a complete code example:

```C
#include <stdio.h>

int main() {
    FILE *fp;
    char str[50];
    
    fp = fopen("data.txt", "r");
    if (fp == NULL) {
        printf("Error opening file.");
    } else {
        fscanf(fp, "%s", str);
        printf("The string in the file is: %s\n", str);
        fclose(fp);
    }
    
    return 0;
}
```

If our `data.txt` file contains the string "Hello World", the output of this code will be:

`The string in the file is: Hello`

## Deep Dive
Reading a text file may seem simple, but there are a few important things to keep in mind. It is important to always check if the file was opened successfully before attempting to read from it. This can be done by checking the return value of `fopen()` - if it is `NULL`, then the file could not be opened.

Another thing to keep in mind is the format specifier used in `fscanf()`. This specifies how the data in the file should be read. For example, if the file contains an integer, we would use `%d` as the format specifier. Using the wrong format specifier can lead to errors and unexpected results.

Lastly, it is important to close the file after we are finished reading from it. If the file is not closed properly, it can lead to memory leaks or the file being corrupted.

## See Also
- [Writing and Reading Files in C](https://www.programiz.com/c-programming/c-file-input-output)
- [File Input/Output in C Tutorial](https://www.cprogramming.com/tutorial/cfileio.html)
- [Reading and Writing Files in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)