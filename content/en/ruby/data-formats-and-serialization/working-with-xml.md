---
date: 2024-01-25 03:39:58.779376-07:00
description: 'How to: Let''s use REXML, included with Ruby, to parse an XML snippet.'
lastmod: '2024-03-13T22:45:00.573463-06:00'
model: gpt-4-1106-preview
summary: Let's use REXML, included with Ruby, to parse an XML snippet.
title: Working with XML
weight: 40
---

## How to:
Let's use REXML, included with Ruby, to parse an XML snippet:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
Output:
```
Name: apple, Color: green
Name: banana, Color: yellow
```

Generating XML is also straightforward:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML Output:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Deep Dive:
XML's roots date back to the 1990s as a simplified subset of SGML for web documents. It's verbose but highly structured, and that's why it's stuck around. It's not the only game in town—JSON and YAML have become popular for their simplicity—but XML holds strong in many enterprise and legacy systems.

Ruby provides a few ways to tackle XML. REXML is an all-Ruby library that's easy to jump into. Nokogiri is a gem that wraps faster C libraries, offering speed and extra features. Choosing between them? Start with REXML for smaller tasks and move to Nokogiri if you need more horsepower.

Under the hood, parsing XML is about translating strings to DOM or SAX models. DOM creates a tree in memory, while SAX streams the document and fires events as it parses. REXML offers both models, but tends to be slower than C extensions like those used by Nokogiri.

## See Also:
- Ruby REXML documentation: https://www.rubydoc.info/stdlib/rexml
- Nokogiri gem: https://nokogiri.org/
- XML specification: https://www.w3.org/XML/
- An introduction to SAX: https://www.saxproject.org/
- YAML vs. JSON vs. XML comparison: https://www.upwork.com/resources/json-vs-xml-vs-yaml
