---
title:                "XML:n käsittely"
date:                  2024-01-26T04:34:05.207443-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML on merkkauskieli, jota käytetään datan tallentamiseen ja siirtämiseen. Ohjelmoijat työskentelevät XML:n parissa mahdollistaakseen sovellusten ja järjestelmien välisen yhteentoimivuuden – ajattele datan vaihtoa ja konfiguraatioasetuksia.

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