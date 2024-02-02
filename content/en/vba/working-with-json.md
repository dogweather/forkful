---
title:                "Working with JSON"
date:                  2024-02-01T13:31:49.809971-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) in VBA is about parsing data formatted as JSON, which is ubiquitous in web APIs and configurations. Programmers do it to exchange data between a web service and a VBA-powered application, like those in Excel.

## How to:

To start, you'll need to add a reference to "Microsoft Scripting Runtime" for dictionary objects and "Microsoft XML, v6.0" for parsing JSON. You can do this in the VBA editor under Tools > References.

Once you've got that set, here's how you can parse a simple JSON string into a dictionary object, which allows for easy access to its values.

```basic
Dim jsonText As String
jsonText = "{""name"":""John Doe"",""age"":30}"

' Parse the JSON string
Dim jsonObject As Object
Set jsonObject = JsonConverter.ParseJson(jsonText)

' Accessing data
Debug.Print "Name: " & jsonObject("name") ' Output: Name: John Doe
Debug.Print "Age: " & jsonObject("age")  ' Output: Age: 30
```

For this example, you'll need a JSON parser like the one from VBA-JSON on GitHub because VBA doesn't natively support JSON. Download the library and include it in your project.

Next, to serialize a VBA object into a JSON string:

```basic
Dim person As New Dictionary
person.Add "name", "Jane Doe"
person.Add "age", 29

Dim jsonString As String
jsonString = JsonConverter.ConvertToJson(person)
Debug.Print jsonString  ' Output: {"name":"Jane Doe","age":29}
```

This code converts a dictionary object representing a person into a JSON string.

## Deep Dive

JSON in VBA sounds a bit like an anachronism as VBA hasn't been significantly updated to embrace modern web standards natively. Historically, VBA interacted with web services using XML, which is more verbose than JSON. JSON's popularity exploded due to its simplicity and lightweight nature, particularly in web applications. However, to use JSON in VBA efficiently, third-party parsers like VBA-JSON became necessary.

While VBA has its utility, especially within Microsoft Office applications, it's often not the first choice for modern web interactions. Languages like Python have extensive support for JSON and web services right out of the box, making them a more seamless option for new projects. Nonetheless, VBA's ability to extend and automate Excel, Access, and other Office products means it remains relevant for a wide range of business applications, especially when integrating with legacy systems or where the Office ecosystem is deeply ingrained.
