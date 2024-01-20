---
title:                "Working with csv"
html_title:           "PowerShell recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma Separated Values) is a file format used to store data in a tabular form, with each row representing a record and columns representing the fields or attributes. This format is widely used in spreadsheets and databases due to its simplicity and compatibility with multiple applications. Programmers often work with CSV files to import, export, and manipulate data efficiently.

## How to:
To work with CSV files in PowerShell, we can use the `Import-Csv` and `Export-Csv` cmdlets. Here's an example of exporting data from a PowerShell object into a CSV file:

```PowerShell
# Create a PowerShell object
$users = @( 
    @{Name="John"; Age=25},
    @{Name="Jane"; Age=30}
)
# Export the object to a CSV file
$users | Export-Csv -Path "C:\Users.csv"
```

This will create a CSV file with two columns, "Name" and "Age", and two rows with the respective values.

To import data from a CSV file into a PowerShell object, we can use the `Import-Csv` cmdlet. Here's an example:

```PowerShell
# Import the CSV file into a PowerShell object
$users = Import-Csv -Path "C:\Users.csv"
# Display the values from the object
$users | ForEach-Object {
    "Name: $($_.Name), Age: $($_.Age)"
}
```

The output will be:

```
Name: John, Age: 25
Name: Jane, Age: 30
```

## Deep Dive:
CSV was first introduced in the early 1970s as a way to transfer data between mainframe computers and was later adopted as a standard file format for spreadsheet applications. Apart from PowerShell, CSV files can also be worked on with other programming languages like Python, Java, and C#.

While working with CSV files, it is essential to consider data types as they can be converted unintentionally. For example, if a field in a CSV file contains a number, but it is enclosed in quotes (e.g., "25"), PowerShell will treat it as a string, not an integer. This can cause issues while performing calculations or comparisons.

## See Also:
- [PowerShell CSV documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [CSV file format  history](https://en.wikipedia.org/wiki/Comma-separated_values#History)