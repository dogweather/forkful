---
title:                "Working with json"
html_title:           "PowerShell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Dealing with JSON is a common task for programmers, as JSON (JavaScript Object Notation) is a lightweight data interchange format that is widely used in web development. JSON simplifies data sharing and serialization with a human-readable syntax that is easy to parse, making it a popular choice for data storage, transfer, and communication.

## How to:

To start working with JSON in PowerShell, we first need to import the `ConvertTo-Json` and `ConvertFrom-Json` cmdlets from the `Microsoft.PowerShell.Utility` module. These cmdlets allow us to convert PowerShell objects to and from JSON format.

```
# Convert a PowerShell object to JSON
$object = @{
    Name = "John Doe"
    Age = 30
    Occupation = "Software Engineer"
}

$object | ConvertTo-Json
```

Output:
```
{
    "Name": "John Doe",
    "Age": 30,
    "Occupation": "Software Engineer"
}
```

```
# Convert JSON to a PowerShell object
$jsonString = '{
    "Name": "Jane Smith",
    "Age": 25,
    "Occupation": "Web Developer"
}'

$jsonString | ConvertFrom-Json
```

Output:
```
Name          : Jane Smith
Age           : 25
Occupation    : Web Developer

```

## Deep Dive:

JSON was first introduced in 2001 as an alternative to XML with a simpler syntax and better performance. Since then, it has become the preferred format for data exchange in many web applications, especially those built with JavaScript. The popularity of JSON can also be attributed to its lightweight structure, making it easier for humans to read and write compared to XML.

Other alternatives to JSON include YAML and CSV, but these formats have their own limitations and are not as widely used as JSON. When working with JSON in PowerShell, it is important to note that not all PowerShell objects can be converted to JSON. Objects that have methods or circular references cannot be converted and will result in an error.

## See Also:

- [Microsoft Docs: Working with JSON data in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/convertto-json?view=powershell-7.1)
- [JSON.org: Introducing JSON](https://www.json.org/json-en.html)