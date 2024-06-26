---
date: 2024-01-26 04:35:13.384955-07:00
description: 'Wie: Lassen Sie uns REXML, das mit Ruby mitgeliefert wird, zum Parsen
  eines XML-Ausschnitts verwenden.'
lastmod: '2024-03-13T22:44:54.424845-06:00'
model: gpt-4-0125-preview
summary: Lassen Sie uns REXML, das mit Ruby mitgeliefert wird, zum Parsen eines XML-Ausschnitts
  verwenden.
title: Arbeiten mit XML
weight: 40
---

## Wie:
Lassen Sie uns REXML, das mit Ruby mitgeliefert wird, zum Parsen eines XML-Ausschnitts verwenden:
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
document.elements.each('fruits/fruit') { |element|
  puts "Name: #{element.attributes['name']}, Farbe: #{element.attributes['color']}"
}
```
Ausgabe:
```
Name: apple, Farbe: green
Name: banana, Farbe: yellow
```

Das Generieren von XML ist ebenfalls unkompliziert:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML-Ausgabe:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Tiefgehend:
Die Wurzeln von XML reichen zurück in die 1990er Jahre als eine vereinfachte Untergruppe von SGML für Webdokumente. Es ist wortreich, aber hochstrukturiert, und das ist der Grund, warum es sich gehalten hat. Es ist nicht die einzige Möglichkeit – JSON und YAML sind aufgrund ihrer Einfachheit beliebt geworden – aber XML ist in vielen Unternehmens- und Legacy-Systemen stark vertreten.

Ruby bietet einige Möglichkeiten, um XML anzugehen. REXML ist eine reine Ruby-Bibliothek, die leicht zu erlernen ist. Nokogiri ist ein Gem, das schnellere C-Bibliotheken umfasst und Geschwindigkeit sowie zusätzliche Funktionen bietet. Die Wahl zwischen ihnen? Beginnen Sie mit REXML für kleinere Aufgaben und wechseln Sie zu Nokogiri, wenn Sie mehr Leistung benötigen.

Unter der Haube ist das Parsen von XML das Übersetzen von Zeichenketten in DOM- oder SAX-Modelle. DOM erstellt einen Baum im Speicher, während SAX das Dokument streamt und Ereignisse auslöst, während es geparst wird. REXML bietet beide Modelle, ist aber tendenziell langsamer als C-Erweiterungen, wie die von Nokogiri verwendeten.

## Siehe auch:
- Ruby REXML-Dokumentation: https://www.rubydoc.info/stdlib/rexml
- Nokogiri Gem: https://nokogiri.org/
- XML-Spezifikation: https://www.w3.org/XML/
- Eine Einführung in SAX: https://www.saxproject.org/
- YAML vs. JSON vs. XML Vergleich: https://www.upwork.com/resources/json-vs-xml-vs-yaml
