---
date: 2024-02-03 19:02:58.472005-07:00
description: "How to: To read or parse JSON in PowerShell, you can use the `ConvertFrom-Json`\
  \ cmdlet. Given a JSON string, this cmdlet converts it into a PowerShell\u2026"
lastmod: '2024-03-13T22:45:00.303574-06:00'
model: gpt-4-0125-preview
summary: To read or parse JSON in PowerShell, you can use the `ConvertFrom-Json` cmdlet.
title: Working with JSON
weight: 38
---

## How to:


### Parsing JSON
To read or parse JSON in PowerShell, you can use the `ConvertFrom-Json` cmdlet. Given a JSON string, this cmdlet converts it into a PowerShell object. 

```powershell
$json = '{"name": "John Doe", "age": 30, "city": "New York"}'
$person = $json | ConvertFrom-Json
$person.name
```

Sample output:

```
John Doe
```

This example demonstrates how to parse a simple JSON string to access properties of the resulting object.

### Generating JSON
To generate JSON from a PowerShell object, you can use the `ConvertTo-Json` cmdlet. This is handy for preparing data to be sent to a web service or saved into a configuration file.

```powershell
$person = [PSCustomObject]@{
    name = "Jane Doe"
    age = 25
    city = "Los Angeles"
}
$json = $person | ConvertTo-Json
Write-Output $json
```

Sample output:

```json
{
    "name":  "Jane Doe",
    "age":  25,
    "city":  "Los Angeles"
}
```

This code snippet creates a PowerShell object and then converts it to a JSON string.
