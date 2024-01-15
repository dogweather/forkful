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

## Why
If you've ever worked with large amounts of data, chances are you've encountered CSV (Comma-Separated Values) files. These files are a popular way of storing data in a tabular format and can be easily manipulated using C#.

## How To
To begin working with CSV in C#, the first step is to include the `System.IO` namespace in your code. Next, you'll need to use the `StreamReader` class to read the CSV file and the `StreamWriter` class to write data to a CSV file. Here's an example of how to read data from a CSV file and output it to the console:

```
using System;
using System.IO;

//Read CSV file
var reader = new StreamReader("data.csv");

//Create and open StreamWriter
var writer = new StreamWriter("output.csv");

//Read until end of file
while (!reader.EndOfStream)
{
    //Read line and split by comma
    var line = reader.ReadLine().Split(',');
    
    //Write data to console
    Console.WriteLine($"Value 1: {line[0]}, Value 2: {line[1]}");
    
    //Write data to output file
    writer.WriteLine($"Value 1: {line[0]}, Value 2: {line[1]}");
}

//Close StreamReader and StreamWriter
reader.Close();
writer.Close();
```

### Sample Output
```
Value 1: John, Value 2: Smith
Value 1: Jane, Value 2: Doe
```

## Deep Dive
Working with CSV files in C# is relatively straightforward, but there are a few things to keep in mind. One important thing to note is that CSV files are typically exported from other programs and may contain different types of data, such as strings, integers, or dates. Therefore, it's important to properly parse the data before using it in your code. Additionally, consider using a library such as `CsvHelper` for easier manipulation and handling of CSV files.

Another tip for working with CSV files is to ensure that your delimiter is consistent. While most CSV files use a comma as the delimiter, some may use a tab or semicolon. This can cause issues when reading or writing data, so it's important to specify the delimiter when working with CSV files.

## See Also
- [Microsoft Docs: CSV Files](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-and-write-to-a-newly-created-data-file)
- [C# Corner: Working with CSV Files in C#](https://www.c-sharpcorner.com/article/working-with-csv-files-in-C-Sharp/)
- [CsvHelper Library](https://joshclose.github.io/CsvHelper/)