---
title:                "C recipe: Creating a temporary file"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a useful technique in many programming scenarios. These files provide a temporary solution for tasks such as caching data, storing temporary results, or passing data between processes.

## How To

To create a temporary file in C, we can use the `tmpfile()` function from the `stdio.h` library. This function takes no arguments and returns a pointer to a FILE object, which can then be used to read and write to the temporary file.

Let's take a look at an example:

```C
#include <stdio.h>

int main() {

    // create a temporary file
    FILE *tempFile = tmpfile();

    // write data to the file
    fprintf(tempFile, "This is some sample data\n");

    // read data from the file
    rewind(tempFile);
    char data[100];
    fgets(data, 100, tempFile);
    printf("%s", data);

    // close the file
    fclose(tempFile);

    return 0;
}
```

We start by including the `stdio.h` library, which contains the `tmpfile()` function. Then, we create a function called `main()` where all our code will be contained.

Inside the `main()` function, we use the `tmpfile()` function to create a temporary file and assign the returned FILE pointer to the variable `tempFile`.

Next, we use the `fprintf()` function to write some sample data to the file. We then use the `rewind()` function to set the file position indicator back to the beginning of the file. Finally, we use the `fgets()` function to read the data from the file and print it to the console.

Once we are done using the temporary file, we can close it using the `fclose()` function. This will automatically delete the temporary file from the system.

Running this program will output:

```
This is some sample data
```

## Deep Dive

Under the hood, the `tmpfile()` function creates a temporary file with a unique filename in the system's temporary directory. This directory can vary depending on the operating system, but it is usually located in the `/tmp` or `\temp` directory.

It is important to note that temporary files created using the `tmpfile()` function will be automatically deleted when the program exits. This makes it a convenient way to create temporary files without having to worry about cleaning them up afterwards.

However, there are situations where we may want to keep the temporary file even after the program has finished running. In those cases, we can use the `tmpnam()` function to create a temporary file with a unique filename and handle its deletion manually.

## See Also

- [Temporary File in C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Creating and Using Temporary Files in C](https://www.geeksforgeeks.org/creating-using-temporary-files-c/)
- [Temporary Directory in Different Operating Systems](https://en.wikipedia.org/wiki/Temporary_folder)