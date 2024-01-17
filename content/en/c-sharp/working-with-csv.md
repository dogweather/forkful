---
title:                "Working with csv"
html_title:           "C# recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

CSV (Comma Separated Values) is a popular file format used for storing and organizing tabular data. In essence, it is a convenient way of representing data that can be easily read and manipulated by both humans and computers. Programmers often work with CSV files because they are lightweight, easy to understand, and compatible with a wide range of programming languages and applications.

## How to:

To work with CSV files in C#, you can use the built-in `System.IO.File` class or the `TextFieldParser` class from `Microsoft.VisualBasic.FileIO` namespace. The following code snippet shows how to read data from a CSV file and display it on the console:

```C#
using System.IO;

string filePath = "data.csv"; 
using (var reader = new StreamReader(filePath))
{
    while (!reader.EndOfStream)
    {
        var line = reader.ReadLine();
        var data = line.Split(',');
        foreach (var column in data)
        {
            Console.Write(column + " ");
        }
        Console.WriteLine();
    }
}
```

The output will be each row of the CSV file displayed on a new line, with the values separated by spaces. 

To write data to a CSV file, you can use the `StreamWriter` class from the `System.IO` namespace. The following code snippet shows how to create a CSV file and write data to it:

```C#
using System.IO;

string filePath = "new_data.csv";
using (var writer = new StreamWriter(filePath))
{
    string[] data = { "1, John, Doe", "2, Jane, Smith", "3, Bob, Johnson" };
    foreach (var row in data)
    {
        writer.WriteLine(row);
    }
}
```

The above code will create a new CSV file named "new_data.csv" and write the data to it. 

## Deep Dive:

CSV files have been in use since the 1980s and were initially developed for use with spreadsheets, hence the term "comma separated values". While commas are the most commonly used delimiter, other delimiters such as tabs, semicolons, and pipes can also be used.

An alternative to working with CSV files is using a database management system. However, CSV files are often preferred for smaller datasets as they require less overhead and can be easily shared between different systems. 

In terms of implementation, working with CSV files involves parsing and formatting the data according to the specified delimiters. The `TextFieldParser` class from the `Microsoft.VisualBasic.FileIO` namespace is specifically designed to handle the parsing of CSV files and offers a variety of methods and properties for reading and writing data.

## See Also:

- [Microsoft Documentation on Working with CSV Files in C#](https://docs.microsoft.com/en-us/dotnet/core/pcs/load-text)
- [CSV File Format Overview](https://support.microsoft.com/en-us/office/import-or-export-text-txt-or-csv-files-5250ac4c-663c-47ce-937b-339e391393ba) from Microsoft Office Support
- [CSV File Format History](https://www.bttger.com/owy/guidebooks/textfiles.html) from Bulkowski's Guidebooks