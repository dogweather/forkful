---
title:                "Working with JSON"
date:                  2024-02-01T21:31:06.991879-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a lightweight data-interchange format that's easy for humans to read and write, and for machines to parse and generate. Programmers use JSON to transmit data between a server and a web application or to store information in a structured, accessible manner within a variety of programming environments, including Visual Basic for Applications (VBA).

## How to:

VBA doesn't natively support JSON parsing or generation, so we'll use a scripting language like JScript (via the ScriptControl object) for parsing JSON strings and building JSON objects. Here's how you can parse a JSON string in VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Name: " & parsed.name & ", Age: " & parsed.age & ", City: " & parsed.city
End Sub
```

To generate JSON, you could use a similar approach, building the JSON string through concatenation:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## Deep Dive

The approaches shown leverage the ScriptControl to handle JSON, essentially outsourcing the work to a JavaScript engine. This is a creative workaround but not necessarily the most efficient or modern way to work with JSON in a VBA context. In more complex applications, this method might become cumbersome and introduce performance overhead or security concerns, since ScriptControl executes in an environment that has full access to the host computer.

Other programming environments, such as Python or JavaScript, offer built-in support for JSON, making them more suited for applications that require extensive JSON manipulation. These languages provide comprehensive libraries that facilitate not only parsing and generation but also querying and formatting of JSON data.

Despite these limitations in VBA, understanding how to work with JSON is vital in a world where web-based data exchange and configuration files are predominantly JSON-formatted. For VBA programmers, mastering these techniques opens up opportunities for integrating with web APIs, interpreting configuration files, or even building simple web applications. However, when projects grow in complexity or demand high performance, developers might consider leveraging more JSON-friendly programming environments.
