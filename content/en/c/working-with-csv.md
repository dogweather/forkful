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

## What & Why?

CSV stands for Comma-Separated Values and is a basic file format used for storing tabular data. Programmers often work with CSV files because they are lightweight and can be easily parsed and manipulated using programming languages like C.

## How to:

To work with CSV files in C, you can use the standard library function fopen() to open the file and then use fscanf() to read the data line by line. Here's an example code:

```C
#include <stdio.h>

int main() {
    FILE *fp;
    char buffer[255];

    fp = fopen("data.csv", "r"); // open the CSV file

    // read and print each line
    while (fscanf(fp, "%s", buffer) != EOF) {
        printf("%s\n", buffer);
    }

    fclose(fp); // close the file
    return 0;
}
```

Assuming we have a CSV file named "data.csv" with the following content:

```
Name, Age, Occupation
John, 25, Developer
Jane, 30, Designer
```

The above code will output:

```
Name,
John,
Jane,
```

## Deep Dive:

CSV files became popular in the 1970s as a form of data exchange between mainframe computers. It was easy to generate and parse, making it a popular choice for organizing and storing data.

There are alternatives to CSV such as JSON and XML, which are more structured and can represent complex data. However, CSV still remains a popular choice for simple data storage and exchange due to its simplicity and compatibility with various programming languages.

When working with CSV, it's important to handle edge cases like escaped characters and missing values. You can also use third-party libraries like Libcsv and CSVParser to make things easier.

## See Also:

- [Official C Documentation](https://devdocs.io/c/)
- [Libcsv](http://libcsv.sourceforge.net/)
- [CSVParser](https://github.com/zieckey/csvparser)