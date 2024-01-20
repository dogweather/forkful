---
title:                "Working with yaml"
html_title:           "PowerShell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML stands for "YAML Ain't Markup Language" and is a human-readable data serialization language. Programmers use YAML to store and share structured data in a simple and readable format. Unlike other markup languages, YAML is designed to be easy to read by both humans and machines, making it a popular choice for configuration files and data exchange.

## How to:
Coding in YAML with PowerShell is easy and straightforward. You can use the ConvertFrom-Yaml cmdlet to convert a YAML file into a PowerShell object and access its properties. Let's look at a simple example:

```
# Sample YAML file
---
name: John Smith
age: 30
title: Software Engineer
```
```
# Convert YAML to PowerShell object
$yamlObject = Get-Content -Path .\sample.yml | ConvertFrom-Yaml
```
```
# Access object properties
$yamlObject.name
$yamlObject.age
$yamlObject.title
```
The output will be:
```
John Smith
30
Software Engineer
```

## Deep Dive:
YAML was first introduced in 2001 as an alternative to XML. It quickly gained popularity due to its simplified syntax and human-friendly structure. It is now used in various programming languages, including PowerShell, for its ease of use and flexibility. YAML is also an open standard, making it compatible with a wide range of tools and platforms.

There are other alternatives to YAML, such as JSON and XML. However, YAML stands out due to its simplicity and readability. It allows for easy nested structures and is less verbose compared to other markup languages. Additionally, PowerShell has built-in support for YAML, making it a convenient choice for working with structured data.

When working with YAML in PowerShell, it's essential to keep in mind that indentation is significant. Similar to Python, the hierarchy of YAML data is defined by the indentation level. Incorrect indentation can lead to syntax errors or unexpected behavior when converting the YAML file into a PowerShell object.

## See Also: