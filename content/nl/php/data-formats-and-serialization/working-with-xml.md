---
title:                "Werken met XML"
aliases: - /nl/php/working-with-xml.md
date:                  2024-01-28T22:11:31.716754-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
XML is een opmaaktaal die wordt gebruikt voor het opslaan en transporteren van gegevens. Programmeurs werken met XML om interoperabiliteit tussen applicaties en systemen mogelijk te maken - denk aan gegevensuitwisseling en configuratie-instellingen.

## Hoe:
XML lezen met SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Herinnering</heading>
                <body>Vergeet dit niet</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Geeft uit: Tove
echo $xml->from;     // Geeft uit: Jani
echo $xml->heading;  // Geeft uit: Herinnering
echo $xml->body;     // Geeft uit: Vergeet dit niet
```

XML schrijven met DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Herinnering');
$body = $dom->createElement('body', 'Vergeet dit niet');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Voorbeelduitvoer:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Herinnering</heading>
  <body>Vergeet dit niet</body>
</note>
```

## Diepgaande verkenning
XML, of EXtensible Markup Language, is sinds de W3C-aanbeveling in 1998 een vast onderdeel geweest in de serialisatie van gegevens. Het is uitvoerig, leesbaar voor mensen en strikt in syntaxis, waardoor het een betrouwbare keuze is voor configuratiebestanden, gegevensuitwisseling en meer. Echter, het is gedeeltelijk overschaduwd door JSON voor web-API's vanwege de eenvoud en lichtgewicht aard.

Programmeurs kiezen vaak voor XML wanneer ze documentvalidatie nodig hebben die wordt geboden door XML-schema's of wanneer ze werken binnen ecosystemen die er al zwaar op vertrouwen (zoals Microsoft Office-bestandsformaten). Het hanteren van XML in PHP is eenvoudig met de SimpleXML-extensie voor basisbewerkingen. Voor complexere manipulatie biedt DOMDocument een robuust scala aan functies die meer controle mogelijk maken, zoals omgaan met namespaces en schema-validatie.

## Zie ook
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
