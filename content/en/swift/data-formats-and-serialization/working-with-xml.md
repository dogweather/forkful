---
aliases:
- /en/swift/working-with-xml/
date: 2024-01-25 03:39:31.391206-07:00
description: "Working with XML means parsing and generating XML data in Swift. Programmers\
  \ do this for data interchange, especially when integrating with systems where\u2026"
lastmod: 2024-02-18 23:09:11.420558
model: gpt-4-1106-preview
summary: "Working with XML means parsing and generating XML data in Swift. Programmers\
  \ do this for data interchange, especially when integrating with systems where\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML means parsing and generating XML data in Swift. Programmers do this for data interchange, especially when integrating with systems where XML is the standard format.

## How to:
Swift provides `XMLParser` and `XMLDocument` for parsing XML data. Here's a snippet to parse a simple XML string:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // Your XMLParserDelegate
    parser.parse()
}
```

You can also generate XML using `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

Sample output:

```xml
<note>
  <to>Tove</to>
</note>
```

## Deep Dive
XML, or Extensible Markup Language, has been around since the late '90s. It's verbose but human-readable, making it a good fit for complex data structures. Swift's XML parsing capabilities aren't as robust as those found in Python's ElementTree or Java's JAXB, but they get the job done for basic needs.

Alternatives like JSON are often preferred in new systems due to their lighter weight and less complex parsers, but XML is still prominent in many enterprise and legacy systems.

When working with XML in Swift, `XMLParser` is a stream-based parser which means it reads through the XML document sequentially. For large XML files, this is memory-efficient. However, if you're looking for simplicity and your XML data is reasonably small, using `XMLDocument` might be more straightforward.

## See Also
- [Apple's XML Parsing Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML Tutorial](https://www.w3schools.com/xml/)
