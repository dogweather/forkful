---
date: 2024-01-25 03:39:47.449718-07:00
description: "Working with XML means parsing, creating, and manipulating XML (eXtensible\
  \ Markup Language) data. Programmers manage XML to handle structured data\u2026"
lastmod: '2024-03-13T22:45:00.379979-06:00'
model: gpt-4-1106-preview
summary: Working with XML means parsing, creating, and manipulating XML (eXtensible
  Markup Language) data.
title: Working with XML
weight: 40
---

## What & Why?
Working with XML means parsing, creating, and manipulating XML (eXtensible Markup Language) data. Programmers manage XML to handle structured data interchange, configuration, and more, due to its platform-neutral nature.

## How to:
Here's a simple way to parse XML using the TinyXML-2 library:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>Hello, World!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

Sample output:

```
Hello, World!
```

And this is how you create an XML file:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("Hello, World!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

This generates an XML file `output.xml` with contents:

```xml
<?xml version="1.0"?>
<root>
    <message>Hello, World!</message>
</root>
```

## Deep Dive
XML has been pivotal in web services and data storage since the late '90s. While JSON and YAML are now more common for config and interop, XML is still huge in many enterprise systems. Parsing XML in C++ can feel old-school with manual DOM/SAX parsing. Thankfully, libraries like TinyXML-2 simplify it. C++ has no built-in XML support; libraries like TinyXML-2, pugixml, or Xerces wrap up the tough bits.

## See Also
- TinyXML-2 Documentation: https://leethomason.github.io/tinyxml2/
- pugixml library: https://pugixml.org/
- Xerces-C++ Parser: https://xerces.apache.org/xerces-c/
- W3C XML Specification: https://www.w3.org/XML/
