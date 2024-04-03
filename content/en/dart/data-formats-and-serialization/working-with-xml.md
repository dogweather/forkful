---
date: 2024-03-08 21:33:43.430952-07:00
description: "How to: Dart does not include built-in support for XML handling in its\
  \ standard library, necessitating the use of third-party packages. One popular\u2026"
lastmod: '2024-03-13T22:44:59.842086-06:00'
model: gpt-4-0125-preview
summary: Dart does not include built-in support for XML handling in its standard library,
  necessitating the use of third-party packages.
title: Working with XML
weight: 40
---

## How to:
Dart does not include built-in support for XML handling in its standard library, necessitating the use of third-party packages. One popular package is `xml`. To use it, you first need to add it to your `pubspec.yaml`:

```yaml
dependencies:
  xml: ^5.0.0 // Use the latest version available
```

Then, import the package in your Dart file:

```dart
import 'package:xml/xml.dart' as xml;
```

**Parsing XML:**

Suppose you have an XML string like this:

```xml
<String name="greeting">Hello, world!</String>
```

You can parse and read the XML as follows:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Outputs: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**Creating XML Documents:**

Creating a new XML document is straightforward with the `xml` package:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**Output**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**Querying and Modifying XML:**

To find or modify elements, you can use XPath-like methods:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // Modifying the 'name' attribute
    greeting.setAttribute('name', 'greeting_modified');
    
    // Adding a new child element
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**Output**:

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

These examples demonstrate basic operations for working with XML in Dart. With the `xml` package, you can parse, create, and manipulate XML documents to suit your application requirements.
