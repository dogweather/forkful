---
title:                "Working with XML"
date:                  2024-01-25T03:39:58.779376-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with XML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with XML means parsing, generating, and manipulating XML (eXtensible Markup Language) documents using code. Programmers do it to interact with many web services, config files, and data interchange formats where XML is the lingua franca.

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
