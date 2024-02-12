---
title:                "Att arbeta med XML"
aliases: - /sv/php/working-with-xml.md
date:                  2024-01-26T04:34:14.448313-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad och varför?
XML är ett märkspråk som används för att lagra och transportera data. Programmerare arbetar med XML för att möjliggöra interoperabilitet mellan applikationer och system - tänk datautbyte och konfigurationsinställningar.

## Hur man gör:
Läsa XML med SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Påminnelse</heading>
                <body>Glöm inte detta</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Ger ut: Tove
echo $xml->from;     // Ger ut: Jani
echo $xml->heading;  // Ger ut: Påminnelse
echo $xml->body;     // Ger ut: Glöm inte detta
```

Skriva XML med DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$rot = $dom->createElement('note');
$dom->appendChild($rot);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$rubrik = $dom->createElement('heading', 'Påminnelse');
$kropp = $dom->createElement('body', 'Glöm inte detta');

$rot->appendChild($to);
$rot->appendChild($from);
$rot->appendChild($rubrik);
$rot->appendChild($kropp);

echo $dom->saveXML();
```

Exempelutmatning:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Påminnelse</heading>
  <body>Glöm inte detta</body>
</note>
```

## Fördjupning
XML, eller Extensible Markup Language, har varit en grundpelare i dataserialisering sedan dess W3C-rekommendation 1998. Det är utförligt, läsbart för människor och strikt i syntax, vilket gör det till ett pålitligt val för konfigurationsfiler, datautbyte och mer. Det har dock delvis överskuggats av JSON för webb-APIer på grund av dess enkelhet och lätta natur.

Programmerare väljer ofta XML när de behöver dokumentvalidering som tillhandahålls av XML-scheman eller när man arbetar inom ekosystem som redan starkt förlitar sig på det (som Microsoft Office-filformat). Hantering av XML i PHP är rakt på sak med tillägget SimpleXML för grundläggande operationer. För mer komplex manipulering erbjuder DOMDocument en robust uppsättning funktioner som tillåter större kontroll, såsom namnrymdshantering och schemavalidering.

## Se även
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
