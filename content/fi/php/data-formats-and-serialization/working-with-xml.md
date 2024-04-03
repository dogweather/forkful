---
date: 2024-01-26 04:34:05.207443-07:00
description: "XML on merkkauskieli, jota k\xE4ytet\xE4\xE4n datan tallentamiseen ja\
  \ siirt\xE4miseen. Ohjelmoijat ty\xF6skentelev\xE4t XML:n parissa mahdollistaakseen\
  \ sovellusten ja\u2026"
lastmod: '2024-03-13T22:44:56.680295-06:00'
model: gpt-4-0125-preview
summary: "XML on merkkauskieli, jota k\xE4ytet\xE4\xE4n datan tallentamiseen ja siirt\xE4\
  miseen."
title: "XML:n k\xE4sittely"
weight: 40
---

## Kuinka:
XML:n lukeminen SimpleXML:n avulla:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Muistutus</heading>
                <body>Älä unohda tätä</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Tulostaa: Tove
echo $xml->from;     // Tulostaa: Jani
echo $xml->heading;  // Tulostaa: Muistutus
echo $xml->body;     // Tulostaa: Älä unohda tätä
```

XML:n kirjoittaminen DOMDocumentilla:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Muistutus');
$body = $dom->createElement('body', 'Älä unohda tätä');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Esimerkkituloste:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Muistutus</heading>
  <body>Älä unohda tätä</body>
</note>
```

## Syväsukellus
XML eli eXtensible Markup Language on ollut tietojen sarjallistamisen peruspilari sen jälkeen, kun se sai W3C:n suosituksen vuonna 1998. Se on monisanainen, ihmisen luettavissa ja tiukka syntaksiltaan, mikä tekee siitä luotettavan valinnan konfiguraatiotiedostoille, tiedonvaihdolle ja muulle. Kuitenkin, se on osittain jäänyt JSON:n varjoon web API:ien osalta sen yksinkertaisuuden ja kevytrakenteisuuden vuoksi.

Ohjelmoijat valitsevat usein XML:n, kun he tarvitsevat dokumenttivalidaatiota, jota XML Schemat tarjoavat, tai kun työskentelevät ekosysteemeissä, jotka jo suurelta osin nojaavat siihen (kuten Microsoft Officen tiedostomuodot). XML:n käsittely PHP:ssä on suoraviivaista SimpleXML-laajennoksen avulla perustoiminnoille. Monimutkaisempia manipulaatioita varten DOMDocument tarjoaa vankan ominaisuuskokonaisuuden, joka mahdollistaa suuremman kontrollin, kuten nimiavaruuskäsittelyn ja skeemavalidaation.

## Katso Myös
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Jäsentimet](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
