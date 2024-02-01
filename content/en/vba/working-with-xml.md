---
title:                "Working with XML"
date:                  2024-02-01T13:32:06.577561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with XML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?

We're diving into how to work with XML (eXtensible Markup Language) in Visual Basic for Applications (VBA). Why? Because XML is everywhere, from web services to config files, and knowing how to parse, read, and write it in VBA can seriously up your scripting game.

## How to:

Let's start simple: reading an XML file. Imagine you've got a file, `example.xml`, that looks something like this:

```xml
<Person>
  <Name>John Doe</Name>
  <Email>johndoe@example.com</Email>
</Person>
```

Here's how you can read and print the name and email from this XML in VBA:

```vb
Sub ReadXML()
    Dim xmlDoc As Object
    Set xmlDoc = CreateObject("MSXML2.DOMDocument")
    
    xmlDoc.Load "C:\path\to\your\example.xml"
    If xmlDoc.ParseError.ErrorCode <> 0 Then
        MsgBox "Error in XML"
    Else
        Dim nameNode As IXMLDOMNode
        Dim emailNode As IXMLDOMNode
        
        Set nameNode = xmlDoc.SelectSingleNode("//Person/Name")
        Set emailNode = xmlDoc.SelectSingleNode("//Person/Email")
        
        MsgBox "Name: " & nameNode.Text & vbCrLf & "Email: " & emailNode.Text
    End If
End Sub
```

This simple script creates an XML object, loads the XML file, checks for errors, and then uses XPath to find the `Name` and `Email` nodes, displaying their values.

Writing XML is just as straightforward. Say you want to create the XML mentioned above:

```vb
Sub WriteXML()
    Dim xmlDoc As Object, rootElem As Object, nameElem As Object, emailElem As Object
    Set xmlDoc = CreateObject("MSXML2.DOMDocument")
    
    Set rootElem = xmlDoc.createElement("Person")
    xmlDoc.appendChild rootElem
    
    Set nameElem = xmlDoc.createElement("Name")
    nameElem.Text = "Jane Doe"
    rootElem.appendChild nameElem
    
    Set emailElem = xmlDoc.createElement("Email")
    emailElem.Text = "janedoe@example.com"
    rootElem.appendChild emailElem
    
    xmlDoc.Save "C:\path\to\your\new_example.xml"
End Sub
```

This will create a new XML file, `new_example.xml`, with a `Person` element containing `Name` and `Email`.

## Deep Dive

XML support in VBA isn't the newest or shiniest tool in the toolbox. It was introduced in a time when XML was becoming the de facto standard for data interchange, around the turn of the millennium. Despite JSON's rise in popularity for web APIs, XML is still widely used in corporate environments, legacy systems, and places where structured document formats (think Docx, Xlsx) are the norm.

VBA handles XML through the `MSXML` library, a relatively old but stable way of working with XML. There are newer, more efficient libraries and methods to work with XML and data interchange formats in more modern programming environments, such as Python's `xml.etree.ElementTree` or JavaScript's `DOMParser`. However, for those embedded in the Microsoft ecosystem, working within Excel, Access, or other Office applications, VBA remains a viable option for XML manipulation. 

Remember though, working with XML in VBA requires familiarity with the Document Object Model (DOM) and XPath query language. It's not as intuitive as JSON in more modern languages, but once you get the hang of it, it's pretty powerful.
