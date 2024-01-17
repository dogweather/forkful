---
title:                "Working with csv"
html_title:           "Java recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) in Java refers to the process of reading and writing data to and from CSV files. CSV files are commonly used to store tabular data, making them a popular choice for data manipulation. Programmers often work with CSV files because they provide a standardized and simple way to store and share data between different applications and programming languages.

## How to:

To work with CSV files in Java, you can use the built-in ```Scanner``` or ```BufferedReader``` classes to read data from a CSV file. Here is a basic code example:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CSVReader {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        String line = "";
        String delimiter = ",";
        
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] data = line.split(delimiter);
                
                // Do something with the data
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

The code above reads data from the "data.csv" file and splits it into an array based on the comma delimiter. From there, you can manipulate the data as needed. 

To write data to a CSV file, you can use the ```BufferedWriter``` class. Here's an example of how you can write data to a CSV file using this class:

```Java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWriter {
    public static void main(String[] args) {
        String csvFile = "output.csv";
        String data = "123,John,Doe";
        
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            bw.write(data);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Deep Dive:

CSV files have been around since the 1970s and were first used in mainframe computers to store data. They gained popularity in the 1990s with the rise of Microsoft Excel and have since become a widely used format for storing data.

While CSV files are popular due to their simplicity and widely supported format, they do have some limitations. For example, they cannot store complex data types such as images or documents. In these cases, other alternatives such as JSON or XML may be a better choice.

Working with CSV files in Java also involves understanding how to handle different delimiters and how to handle errors or missing data. Additionally, there are third-party libraries available for more advanced CSV parsing and manipulation, such as the OpenCSV library.

## See Also:

- [Java CSV library - OpenCSV](http://opencsv.sourceforge.net/)
- [History of CSV files](https://en.wikipedia.org/wiki/Comma-separated_values)