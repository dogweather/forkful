---
title:                "Working with csv"
html_title:           "C++ recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (comma-separated values) in C++ involves reading and writing data in a specific format where each field is separated by a comma. Programmers often work with CSV files because they are a convenient and efficient way to store and manipulate large amounts of data, such as spreadsheets or databases.

## How to:
To work with CSV files in C++, you can use the built-in library function ```getline``` to read each line of the file and ```stringstream``` to split the line by commas into separate strings. Here's an example of reading data from a CSV file and printing it out:
```C++
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;

int main() {
    ifstream file("data.csv"); // open file
    string line;
    while (getline(file, line)) { // read each line
        stringstream ss(line); // split the line into chunks
        string field;
        while (getline(ss, field, ',')) { // read each field
            cout << field << " ";
        }
        cout << endl;
    }
    return 0;
}
```
**Sample output:**
```
John Doe 30
Jane Smith 25
Bob Johnson 40
```

## Deep Dive:
CSV was first introduced in the 1970s as a way to transfer data between different computer programs. It remains popular today due to its simple and universal format. Alternatives to CSV include other text-based formats such as TSV (tab-separated values) or XML. These may have specific advantages for certain use cases, but CSV is widely used due to its simplicity.

When working with CSV files, it's important to handle special characters and escaping properly, as well as dealing with different line endings on different operating systems. The C++ standard library provides functions such as ```getline``` and ```getchar``` to handle these issues.

## See Also:
- [C++ reference for getline function](https://www.cplusplus.com/reference/string/string/getline/)
- [Comparison of different file formats](https://www.computerhope.com/issues/ch001356.htm)