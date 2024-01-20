---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means dealing with plain text files that store tabular data. Programmers use CSV because it's simple and compatible across various systems, perfect for exchanging data between different software.

## How to:

Here’s a chunk of code that reads a CSV file and prints its content.

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

int main() {
    std::string line, cell;
    std::vector<std::vector<std::string>> csvData;
    std::ifstream file("example.csv");

    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::vector<std::string> rowData;
        
        while (std::getline(lineStream, cell, ',')) {
            rowData.push_back(cell);
        }
        csvData.push_back(rowData);
    }
    
    for (const auto& row : csvData) {
        for (const auto& col : row) {
            std::cout << col << " ";  // Depending on your CSV structure, adjust the delimiter.
        }
        std::cout << std::endl;
    }
    return 0;
}
```

Sample output for a CSV containing names and ages:
```
John 25
Jane 28
```

## Deep Dive

CSV has been around since the early 1970s. It's the go-to format for simple data export and import but isn't great for complex hierarchical data, which XML and JSON handle better. C++ doesn't have built-in CSV support, but handling files and strings is straightforward. You deal with standard I/O and string manipulation, while looking out for corner cases like quotes and commas within cells. Libraries like `libcsv` and `Boost.Tokenizer` can simplify tasks if you're dealing with more complex CSV files.

## See Also

- [RFC 4180](https://tools.ietf.org/html/rfc4180), the common format and MIME type for CSV files.
- [C++ reference for I/O](http://www.cplusplus.com/reference/fstream/)
- [The Boost C++ Libraries](https://www.boost.org/)
- [10 minutes to pandas - CSV handling with Python for comparison](https://pandas.pydata.org/pandas-docs/stable/user_guide/10min.html)