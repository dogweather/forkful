---
date: 2024-01-25 03:39:37.908801-07:00
description: "XML is a markup language used for storing and transporting data. Programmers\
  \ work with XML to enable interoperability between applications and systems -\u2026"
lastmod: '2024-03-13T22:45:00.189434-06:00'
model: gpt-4-1106-preview
summary: XML is a markup language used for storing and transporting data.
title: Working with XML
weight: 40
---

## What & Why?
XML is a markup language used for storing and transporting data. Programmers work with XML to enable interoperability between applications and systems - think data exchange and configuration settings.

## How to:
Reading XML with SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Don't forget this</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Outputs: Tove
echo $xml->from;     // Outputs: Jani
echo $xml->heading;  // Outputs: Reminder
echo $xml->body;     // Outputs: Don't forget this
```

Writing XML with DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Don't forget this');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Sample output:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget this</body>
</note>
```

## Deep Dive
XML, or eXtensible Markup Language, has been a staple in data serialization since its W3C recommendation in 1998. It's verbose, human-readable, and strict in syntax, making it a reliable choice for configuration files, data interchange, and more. However, it's been partially overshadowed by JSON for web APIs due to its simplicity and light-weight nature.

Programmers often pick XML when they need document validation provided by XML Schemas or when working within ecosystems that already heavily rely on it (like Microsoft Office file formats). Handling XML in PHP is straightforward with the SimpleXML extension for basic operations. For more complex manipulation, DOMDocument provides a robust set of features that allow for greater control, such as namespace handling and schema validation.

## See Also
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
