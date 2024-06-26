---
date: 2024-02-03 19:03:13.807014-07:00
description: 'How to: To read from a CSV file, use the `Import-Csv` cmdlet. This cmdlet
  reads the file and converts it into custom PowerShell objects for each row.'
lastmod: '2024-03-13T22:45:00.304439-06:00'
model: gpt-4-0125-preview
summary: To read from a CSV file, use the `Import-Csv` cmdlet.
title: Working with CSV
weight: 37
---

## How to:


### Reading a CSV File
To read from a CSV file, use the `Import-Csv` cmdlet. This cmdlet reads the file and converts it into custom PowerShell objects for each row.

```powershell
# Importing a CSV file
$data = Import-Csv -Path "C:\Data\users.csv"
# Display the content
$data
```

**Sample Output:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### Writing to a CSV File
Conversely, to write data into a CSV file, the `Export-Csv` cmdlet is used. This cmdlet takes input objects and converts them into a CSV format.

```powershell
# Creating an object to export
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# Exporting to a CSV file
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

After executing, a file named `new_users.csv` is created with the provided data.

### Filtering and Manipulating CSV Content
To filter or manipulate the data from a CSV file, use PowerShell's object manipulation capabilities. For instance, to select only users above a certain age and from a specific city:

```powershell
# Importing and filtering data
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# Display filtered data
$filteredData
```

**Sample Output:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### Using Third-Party Libraries
While PowerShell's native cmdlets are usually sufficient for common tasks, more complex operations might benefit from third-party libraries or tools. However, for standard CSV manipulation, such as reading, writing, filtering, or sorting, PowerShell's built-in cmdlets like `Import-Csv` and `Export-Csv` usually offer robust functionality without the need for additional libraries.
