---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) means reading and writing data in a simple, text-based format — one that's universal and spreadsheet-friendly. Programmers use CSV for its simplicity and interoperability when exchanging tabular data between systems.

## How to

### Reading CSV Files
```C#
using System;
using System.IO;

class ReadCSVExample
{
    static void Main()
    {
        string path = "data.csv";
        using (var reader = new StreamReader(path))
        {
            while (!reader.EndOfStream)
            {
                var line = reader.ReadLine();
                var values = line.Split(',');
                // Now do something with the values, e.g., print them
                Console.WriteLine(String.Join(" | ", values));
            }
        }
    }
}
```
**Sample Output:**
```
John | Doe | johndoe@example.com
Jane | Smith | janesmith@example.com
```

### Writing CSV Files
```C#
using System;
using System.IO;

class WriteCSVExample
{
    static void Main()
    {
        string path = "output.csv";
        var records = new[]
        {
            new[] {"Name", "Age", "Email"},
            new[] {"Alice", "23", "alice@example.com"},
            new[] {"Bob", "30", "bob@example.com"}
        };

        using (var writer = new StreamWriter(path))
        {
            foreach (var record in records)
            {
                var line = String.Join(",", record);
                writer.WriteLine(line);
            }
        }
        Console.WriteLine($"Data written to {path}");
    }
}
```
**Sample Output:**
```
Data written to output.csv
```

## Deep Dive

CSV's been around since the early days of computing, bridging the gap between diverse systems. It's not perfect — lacks standard encoding for characters and doesn't support multi-line fields well without a robust parser. That's where formats like JSON and XML stride in, offering more complexity but better structure for hierarchical data.

Under the hood, you're usually manipulating strings, either built-in `string` methods or libraries like `CsvHelper` can add extra muscle to your CSV handling, providing more features and handling edge cases gracefully. Remember, there's no native CSV handling in .NET, so you're on your own with string manipulation or you can opt for a third-party library.

## See Also

For more in-depth CSV manipulation in C#:
- [CsvHelper Library](https://joshclose.github.io/CsvHelper/)
- [Microsoft’s documentation on `StreamReader`](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)

Learn more about alternatives to CSV:
- [Understanding JSON](https://www.json.org/json-en.html)
- [XML in a Nutshell](https://www.w3schools.com/xml/xml_whatis.asp)