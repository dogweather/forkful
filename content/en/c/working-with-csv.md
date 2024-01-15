---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV, or comma-separated values, is a common data format used for storing and exchanging tabular data. It is simple, lightweight, and widely supported, making it a popular choice for many data-related tasks such as importing and exporting data from databases or working with spreadsheets. In this article, we will explore how to work with CSV files in C, the current version of the C programming language.

## How To
To work with CSV files in C, we will use the standard library functions `fopen()`, `fscanf()`, and `fclose()`. `fopen()` is used to open the CSV file, `fscanf()` is used to read data from the file, and `fclose()` is used to close the file once we are done with it. Let's take a look at an example:

```
#include <stdio.h>

int main() {
  // Open the CSV file for reading
  FILE *csv_file = fopen("sample.csv", "r");

  // Check if the file was opened successfully
  if(csv_file == NULL) {
    printf("Error opening file!");
    return 1;
  }

  // Read data from the file and print it to the console
  char name[10];
  int age;
  float height;
  while(fscanf(csv_file, "%s,%d,%f", name, &age, &height) != EOF) {
    printf("Name: %s, Age: %d, Height: %.2f\n", name, age, height);
  }

  // Close the file
  fclose(csv_file);

  return 0;
}
```

In this example, we are opening a CSV file called "sample.csv" for reading, using `fscanf()` to read data from the file in a specific format, and using a loop to print the data to the console. Notice that we are using `%s` for a string, `%d` for an integer, and `%f` for a float, and separating each data item with a comma as per the CSV format.

## Deep Dive
Now let's dive deeper into working with CSV files in C. There are a few things to keep in mind when working with CSV files:

- Make sure the data types used in `fscanf()` match the data types in the CSV file.
- Handle errors gracefully by checking for `NULL` when using `fopen()` and `EOF` when using `fscanf()`.
- Use the correct file mode depending on what you want to do with the file. For example, use "r" for reading, "w" for writing, and "a" for appending.
- Handle special characters carefully. For example, if the CSV data contains commas, you will need to make sure they are properly escaped or quoted.

Also, keep in mind that the standard library functions for working with files may not be the most efficient for large CSV files. You may want to consider using third-party libraries or implementing your own functions for optimal performance.

## See Also
- [The C Programming Language](https://en.wikipedia.org/wiki/C_(programming_language))
- [Working with CSV Files in C](https://www.codeproject.com/Articles/417352/Working-with-CSV-Files-in-C)
- [CSV File Format](https://en.wikipedia.org/wiki/Comma-separated_values)