---
title:                "Working with XML"
date:                  2024-01-25T03:40:13.607897-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with XML involves parsing, manipulating, and generating XML documents, which are used for data exchange due to their structured and widespread format. Programmers handle XML to interface with countless systems where XML is the lingua franca of data.

## How to:
Gleam doesn't natively support XML, so we'll use an external library like `gleam_xml`. First, add it to your `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Now, parse and create XML:

```rust
import gleam/xml

// Parse XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Create XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

Sample output for `xml.render(node)` is:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Deep Dive
XML stands for eXtensible Markup Language, a spec from the W3C as a sister to HTML. It's been around since the late '90s. For Gleam, handling XML feels a bit like a step back in time. JSON and Protocol Buffers are trendier, but XML's extensive use in legacy systems and certain industries means it's still relevant.

Alternatives like `xmerl` exist in the Erlang ecosystem; however, the `gleam_xml` library provides a more idiomatic approach for Gleam users. It's built on top of existing Erlang libraries but exposes a Gleam-friendly API. The Gleam approach to XML aims for simplicity and safety, reducing the boilerplate and emphasizing type security.

Implementation-wise, XML libraries including `gleam_xml` typically provide DOM-like structures. This entails nodes, attributes, and nested elements, leveraging Erlang's pattern matching and concurrency models to handle potentially large and complex documents.

## See Also
- The `gleam_xml` library on Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Official XML standard by W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Comprehensive XML tutorial: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlang's `xmerl` documentation for XML processing: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)