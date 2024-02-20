---
date: 2024-01-26 04:33:36.498239-07:00
description: "XML ist eine Auszeichnungssprache, die zum Speichern und Transportieren\
  \ von Daten verwendet wird. Programmierer arbeiten mit XML, um die Interoperabilit\xE4\
  t\u2026"
lastmod: 2024-02-19 22:05:12.927709
model: gpt-4-0125-preview
summary: "XML ist eine Auszeichnungssprache, die zum Speichern und Transportieren\
  \ von Daten verwendet wird. Programmierer arbeiten mit XML, um die Interoperabilit\xE4\
  t\u2026"
title: Arbeiten mit XML
---

{{< edit_this_page >}}

## Was & Warum?
XML ist eine Auszeichnungssprache, die zum Speichern und Transportieren von Daten verwendet wird. Programmierer arbeiten mit XML, um die Interoperabilität zwischen Anwendungen und Systemen zu ermöglichen - denken Sie an Datenübertragung und Konfigurationseinstellungen.

## Wie geht das:
XML mit SimpleXML lesen:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Erinnerung</heading>
                <body>Vergiss das nicht</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Ausgabe: Tove
echo $xml->from;     // Ausgabe: Jani
echo $xml->heading;  // Ausgabe: Erinnerung
echo $xml->body;     // Ausgabe: Vergiss das nicht
```

XML mit DOMDocument schreiben:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Erinnerung');
$body = $dom->createElement('body', 'Vergiss das nicht');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Beispiel-Ausgabe:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Erinnerung</heading>
  <body>Vergiss das nicht</body>
</note>
```

## Vertiefung
XML oder eXtensible Markup Language ist seit seiner W3C-Empfehlung im Jahr 1998 ein Grundpfeiler in der Daten-Serialisierung. Es ist ausführlich, menschenlesbar und streng in der Syntax, was es zu einer zuverlässigen Wahl für Konfigurationsdateien, Datenaustausch und mehr macht. Allerdings wurde es teilweise von JSON für Web-APIs aufgrund seiner Einfachheit und Leichtigkeit überschattet.

Programmierer entscheiden sich oft für XML, wenn sie Dokumentenvalidierung benötigen, die durch XML Schemas bereitgestellt wird, oder wenn sie in Ökosystemen arbeiten, die bereits stark darauf angewiesen sind (wie Microsoft Office-Dateiformate). Die Handhabung von XML in PHP ist mit der SimpleXML-Erweiterung für grundlegende Operationen unkompliziert. Für komplexere Manipulationen bietet DOMDocument einen robusten Satz von Funktionen, die eine größere Kontrolle ermöglichen, wie die Behandlung von Namensräumen und Schema-Validierung.

## Siehe Auch
- [PHP: SimpleXML](https://www.php.net/manual/de/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/de/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
