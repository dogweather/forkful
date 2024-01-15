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

## Why
CSV (Comma Separated Values) is a popular file format used for storing tabular data. It is widely used in data analysis, data exchange, and data storage due to its simple and universal structure. As a result, being able to work with CSV files is a valuable skill for any programmer.

## How To
To start working with CSV files in C++, we need to include the `<fstream>` library and declare our input file stream and CSV parser objects as follows:
```C++
#include <fstream> 
#include <iostream>
#include <sstream> 
using namespace std;
  
ifstream input_file("data.csv"); 
string line; 
string cell; 
stringstream ss;
```
Next, we can use a `while` loop to read each line from the CSV file and then use a `while` loop nested within it to delimit each line into separate cells using the `getline()` function. The parsed data can then be stored in a 2D vector for easy access and manipulation. Here's an example code:
```C++
vector<vector<string>> data; 
  
while(getline(input_file, line)) 
{ 
    vector<string> row; 
    ss.str(line); 
  
    while(getline(ss, cell, ',')) 
    { 
        row.push_back(cell); 
    } 
  
    data.push_back(row); 
    ss.clear(); 
}
```
To access a specific cell in the data, we can use the index of the row and column, like `data[2][3]` for the fourth cell in the third row. Now, let's say we want to display the data in a formatted table, we can use a `for` loop to iterate through the vector and print out the data using `cout` as shown below:
```C++
for(int i = 0; i < data.size(); i++) 
{ 
    for(int j = 0; j < data[i].size(); j++) 
    { 
        cout << data[i][j] << " | "; 
    } 
    cout << endl; 
} 
```
Here's a sample output for reference:
```
ID | Name | Age | Occupation | 
01 | John | 25 | Student | 
02 | Sarah | 28 | Teacher | 
03 | Peter | 30 | Engineer |
```

## Deep Dive
While CSV files are simple to work with, it's essential to keep in mind a few things. Firstly, not all CSV files are the same. Some may use a different delimiter, such as a semicolon (;) or a tab, instead of a comma. So it's best to check the file beforehand and adjust the code accordingly. Secondly, CSV files may also contain empty cells, so it's crucial to handle those cases using conditionals or error handling. Lastly, for larger files, it's more efficient to use a memory-mapped file instead of storing all the data in a vector.

## See Also
- [C++ Documentation](https://en.cppreference.com/w/)
- [CSV File Formats](https://en.wikipedia.org/wiki/Comma-separated_values)
- [Working with CSV Files in C++](https://www.javatpoint.com/csv-parser-in-cpp)