---
date: 2024-01-25 03:40:03.096300-07:00
description: "Working with XML on Arduino involves parsing and manipulating XML data,\
  \ usually coming from web APIs or configuration files. Programmers do it to\u2026"
lastmod: '2024-02-25T18:49:56.780317-07:00'
model: gpt-4-1106-preview
summary: "Working with XML on Arduino involves parsing and manipulating XML data,\
  \ usually coming from web APIs or configuration files. Programmers do it to\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML on Arduino involves parsing and manipulating XML data, usually coming from web APIs or configuration files. Programmers do it to integrate with services that use XML for data exchange or to store data in a structured, human-readable format.

## How to:
We'll use the `XMLWriter` library to create XML and the `tinyxml2` library to parse it. Install the libraries first via the Library Manager in your Arduino IDE.

Creating an XML document:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Using Serial to output
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

Decoding an XML string:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Sample output:

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## Deep Dive
XML, or Extensible Markup Language, is a markup language that defines a set of rules for encoding documents in a format that is both human-readable and machine-readable. It's been around since the late '90s and is used extensively in various fields, especially where platform-independent data exchange is needed. Arduino's limited memory resources make working with XML more challenging than on a PC. Hence, lightweight libraries are crucial. Although JSON has gained popularity for data exchange due to its simpler syntax and smaller footprint, XML is still used widely, especially when dealing with legacy systems or applications that require document validation via schemas. Key to Arduino XML implementation is stream parsing, which reads the document in segments to keep memory usage low.

## See Also
- [TinyXML-2 Library Documentation](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON Library](https://arduinojson.org/) for an alternative when working with JSON data.
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/) for general XML learning.
- [W3C XML Specification](https://www.w3.org/XML/) for the official XML standards and recommendations.
