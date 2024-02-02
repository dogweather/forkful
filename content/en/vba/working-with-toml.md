---
title:                "Working with TOML"
date:                  2024-02-01T13:32:05.772403-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?

TOML, short for Tom's Obvious, Minimal Language, is a handy format for configuration files. Programmers use it because it's easy to read, write, and works well for defining key-value pairs, eventually making configuration management a breeze in various applications, including those developed with Visual Basic for Applications (VBA).

## How to:

Unfortunately, VBA doesn't come with built-in support for parsing TOML files directly. Fear not, though—you can still manage TOML files by either calling external parsers or doing a bit of manual string manipulation. Here's a simple example to get you started with the latter:

Imagine you have the following TOML content in a file named `config.toml`:
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8002 ]
connection_max = 5000
enabled = true
```

First, you'd want to read this file into VBA. Here's a basic function to read file contents into a string:

```basic
Function ReadFile(filePath As String) As String
    Dim text As String
    Dim fileNo As Integer
    fileNo = FreeFile
    
    Open filePath For Input As #fileNo
    text = Input(LOF(fileNo), fileNo)
    Close #fileNo
    
    ReadFile = text
End Function
```

Next, you'll parse the TOML content. A fully fledged parser is complex, but here's a snippet to find the `server` value within the `database` section:

```basic
Function GetServerValue(tomlContent As String) As String
    Dim startPos As Long, endPos As Long, lineContent As String
    startPos = InStr(tomlContent, "[database]") ' Find database section
    endPos = InStr(startPos, tomlContent, "server") ' Find server key within section
    
    lineContent = Mid(tomlContent, endPos, InStr(endPos, tomlContent, vbNewLine) - endPos)
    
    GetServerValue = Trim(Split(lineContent, "=")(1)) ' Extract and return the value
End Function
```

To use these functions:

```basic
Sub DemoReadTOML()
    Dim fileContents As String
    fileContents = ReadFile("C:\path\to\config.toml")
    
    MsgBox "Server value: " & GetServerValue(fileContents)
End Sub
```

The message box will display something like `Server value: "192.168.1.1"`, showing you've successfully extracted information from a TOML file.

## Deep Dive

The TOML format emerged as an alternative to XML and JSON for configuration files, designed for unambiguous parsing with a clear, minimal syntax. While VBA doesn't natively support TOML, as demonstrated, it's not beyond reach to work with it through custom parsing—albeit with a bit of effort. Bear in mind, for large or complex TOML files, this manual parsing can become impractical. In such cases, leveraging external tools with command line interfaces (CLIs) might be a more efficient route, or even considering an alternative configuration format like JSON, which VBA can parse more readily with ScriptControl's JSON parser or using a library like JsonConverter for more sophisticated projects.
