---
title:                "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Why 

CSV (Comma Separated Values) files are a popular format for storing and exchanging data. As a programmer, knowing how to work with CSV files can greatly enhance your ability to process and analyze large datasets. In this blog post, we will discuss the basics of working with CSV files in C programming.

## How To

To start working with CSV files in C, we first need to include the necessary header files. The ```stdio.h``` header file provides the functions we need for file input/output, while ```stdlib.h``` provides the ```exit()``` function which we will use to handle errors.

We begin by opening the CSV file using the ```fopen()``` function and specifying the file mode as "r" for reading. Before parsing the data, we need to check if the file was opened successfully, and if not, we use the ```exit()``` function to exit the program.

Next, we need to read the data from the CSV file. We can use the ```fgets()``` function to read each line of the file and store it in a buffer. We can then use the ```strtok()``` function to split the line into individual fields, using the comma as the delimiter.

Here's a sample code snippet that reads a CSV file and prints the first field of each row:

```C
#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 100 // maximum length of a line in the CSV file

int main() {
    FILE *csv_file = fopen("data.csv", "r"); // open CSV file for reading
    if (csv_file == NULL) { // check if the file was opened successfully
        printf("Error opening file.");
        exit(1); // exit program with error code
    }

    char buffer[MAX_LEN]; // buffer to store each line of the file

    // read each line of the file until the end is reached
    while (fgets(buffer, MAX_LEN, csv_file) != NULL) {
        // split the line into fields using the comma as the delimiter
        char *token = strtok(buffer, ",");
        // print the first field of each line
        printf("%s\n", token);
    }

    fclose(csv_file); // close the file
    return 0;
}
```

Given the following data in the CSV file:

```csv
Name,Age,City
John,25,New York
Emily,32,London
Mark,28,Paris
```

The output of the above code would be:

```
Name
John
Emily
Mark
```

## Deep Dive

In addition to reading from CSV files, we can also write data to them using the ```fprintf()``` function. This allows us to create new CSV files or modify existing ones.

When working with CSV files, it's important to handle different data types appropriately. For example, if we want to read in numerical data, we may need to convert it from a string to an integer or float using functions like ```atoi()``` or ```atof()```.

It's also important to handle special cases, such as empty fields or fields with extra spaces. We can use the ```strcmp()``` function to compare strings and check for these cases.

Working with large CSV files may also require us to use memory management techniques to avoid running out of memory. In these cases, we can use the ```malloc()``` and ```free()``` functions to dynamically allocate and free memory as needed.

## See Also

- [C Programming: File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Working with CSV Files in C](https://www.codementor.io/@info658/working-with-csv-files-in-c-du1089p95)