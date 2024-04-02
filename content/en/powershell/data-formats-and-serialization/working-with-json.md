---
date: 2024-02-03 19:02:58.472005-07:00
description: "PowerShell's integration with JSON (JavaScript Object Notation) is about\
  \ parsing (reading) and generating (writing) JSON data, a common format for data\u2026"
lastmod: '2024-03-13T22:45:00.303574-06:00'
model: gpt-4-0125-preview
summary: "PowerShell's integration with JSON (JavaScript Object Notation) is about\
  \ parsing (reading) and generating (writing) JSON data, a common format for data\u2026"
title: Working with JSON
weight: 38
---

## What & Why?

PowerShell's integration with JSON (JavaScript Object Notation) is about parsing (reading) and generating (writing) JSON data, a common format for data exchange on the web. Programmers work with JSON to interact with web APIs, configuration files, or to facilitate data interchange between different languages and platforms due to its lightweight and language-independent nature.

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
