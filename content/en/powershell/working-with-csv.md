---
title:                "Working with CSV"
date:                  2024-01-19
simple_title:         "Working with CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma-Separated Values) involves handling text data split by commas into rows and columns. Programmers work with CSV for data exchange between programs and systems due to its simplicity and widespread support.

## How to:

### Import a CSV File
```PowerShell
$data = Import-Csv -Path "path\to\yourfile.csv"
$data
```
**Sample Output:**
```
Name        Occupation    Location
----        ----------    --------
John Doe    Developer     New York
Jane Smith  Analyst       San Francisco
```

### Export to a CSV File
```PowerShell
$data | Export-Csv -Path "path\to\newfile.csv" -NoTypeInformation
```
**Creates "newfile.csv" with the data from `$data`.**

### Add a Row to CSV Data
```PowerShell
$newRow = [PSCustomObject]@{
    Name       = 'Emily Clark'
    Occupation = 'Designer'
    Location   = 'Austin'
}
$data += $newRow
$data | Export-Csv -Path "path\to\yourfile.csv" -NoTypeInformation
```

### Select Specific Columns
```PowerShell
$data | Select-Object Name, Location
```
**Sample Output:**
```
Name        Location
----        --------
John Doe    New York
Jane Smith  San Francisco
Emily Clark Austin
```

## Deep Dive

Historically, CSV files have roots in early computing as a straightforward way to organize table data without needing complex file formats. Alternatives, like XML and JSON, offer richer data structures, but CSV shines for tabular data due to its readability, low overhead, and ease of editing with simple text editors. In PowerShell, `Import-Csv` and `Export-Csv` cmdlets encapsulate the implementation details, handling file IO and data conversion to and from .NET objects.

## See Also

- [PowerShell Documentation on Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- [PowerShell Documentation on Export-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)
