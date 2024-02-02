---
title:                "Working with YAML"
date:                  2024-02-01T13:31:47.909551-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML ain't Markup Language (YAML) is about writing data in a format that's readable by humans and computers. Programmers work with YAML because it's easier to read and write than other formats like XML or JSON, especially for configuration files or data interchange.

## How to:

Visual Basic for Applications (VBA) doesn't have built-in support for parsing or generating YAML. However, you can work around this by using a combination of dictionary objects and string manipulation, or by leveraging external libraries if permissible. Here is a simple example of reading a very basic YAML string, assuming it has a straightforward key-value structure:

```basic
Sub WorkWithYAML()
    Dim yamlString As String
    yamlString = "name: John Doe" & vbCrLf & "age: 34" & vbCrLf & "city: New York"
    
    Dim yamlLines() As String
    yamlLines = Split(yamlString, vbCrLf)
    
    Dim yamlDict As Object
    Set yamlDict = CreateObject("Scripting.Dictionary")
    
    Dim line As Variant
    For Each line In yamlLines
        Dim keyValue() As String
        keyValue = Split(line, ": ")
        yamlDict.Add keyValue(0), keyValue(1)
    Next line
    
    Debug.Print "Name: " & yamlDict("name")
    Debug.Print "Age: " & yamlDict("age")
    Debug.Print "City: " & yamlDict("city")
End Sub
```

This code splits the YAML content into lines, then splits each line by ": " to get keys and values, which are added to a dictionary object. Unfortunately, this approach is very basic and doesn't handle complex YAML features such as nested structures, lists, or more sophisticated data types.

## Deep Dive

Working with YAML in VBA has its limitations given VBA's age and the lack of modern library support directly within VBA. Historically, VBA has been used more with XML and JSON when dealing with structured data due to their support in Microsoft technologies. However, as YAML gains popularity for its readability and simplicity, especially in configuration and deployment scenarios, there's a growing need to work with YAML in environments not traditionally designed for it, like VBA.

While the above approach gets you started with simple YAML strings, it's far from the comprehensive solution you might find in languages like Python, which has libraries like PyYAML that offer robust YAML parsing and generating capabilities.

If you're working on a project that heavily relies on YAML and VBA, consider whether alternative solutions are viable. For instance, transforming YAML data into JSON format externally and then using a JSON parser in VBA, or calling out to a command-line tool from VBA that can convert or process YAML files and return the results in a format VBA can more easily handle. 

Remember, adopting the right tool for the job is crucial, and while it's possible to manipulate YAML to some extent in VBA, exploring other programming environments might offer a smoother path forward for your project needs.
