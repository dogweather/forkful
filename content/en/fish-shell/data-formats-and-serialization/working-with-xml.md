---
date: 2024-01-25 03:39:27.877405-07:00
description: "Working with XML means wrangling data in a pervasive, structured format\
  \ used in configurations, messaging, and more. Programmers manipulate XML to read,\u2026"
lastmod: 2024-02-19 22:05:18.956925
model: gpt-4-1106-preview
summary: "Working with XML means wrangling data in a pervasive, structured format\
  \ used in configurations, messaging, and more. Programmers manipulate XML to read,\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML means wrangling data in a pervasive, structured format used in configurations, messaging, and more. Programmers manipulate XML to read, write, update, and query data—vital for interoperability in tons of apps and services.

## How to:
Fish doesn't have built-in XML parsing, so you'll lean on external tools like `xmllint` or `xmlstarlet`. Here’s a snippet to read values:

```fish
# Parse XML using xmlstarlet
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Output:
```
Hello World
```

To edit XML, use this:

```fish
# Edit XML element using xmlstarlet
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

Output:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## Deep Dive:
XML's been around since the late '90s, crafted for readability and machine-friendliness. While JSON's usurped some of XML's popularity due to simplicity, XML remains entrenched where document validation and namespaces are key. 

Alternatives? Sure—JSON, YAML, or even binary formats like Protocol Buffers for those performance-intensive apps. But XML's schema and XSLT (for XML transformations) can be deal-breakers for complex scenarios where robustness matters.

Under the hood, tools like `xmlstarlet` wrap powerful libraries like libxml2, handing you XPath and XQuery for fine-grained XML tinkering. These aren't just XML tools but gateways to DOM manipulation, as you'd apply similar concepts in any language that touches XML.

## See Also:
- [xmlstarlet Documentation](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [XPath and XQuery Functions and Operators](https://www.w3.org/TR/xpath-functions/)
