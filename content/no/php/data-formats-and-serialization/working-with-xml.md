---
date: 2024-01-26 04:34:09.263746-07:00
description: "XML er et merkespr\xE5k brukt for lagring og transport av data. Programmerere\
  \ arbeider med XML for \xE5 muliggj\xF8re interoperabilitet mellom applikasjoner\
  \ og\u2026"
lastmod: '2024-02-25T18:49:39.084212-07:00'
model: gpt-4-0125-preview
summary: "XML er et merkespr\xE5k brukt for lagring og transport av data. Programmerere\
  \ arbeider med XML for \xE5 muliggj\xF8re interoperabilitet mellom applikasjoner\
  \ og\u2026"
title: "\xC5 jobbe med XML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
XML er et merkespråk brukt for lagring og transport av data. Programmerere arbeider med XML for å muliggjøre interoperabilitet mellom applikasjoner og systemer - tenk datautveksling og konfigurasjonsinnstillinger.

## Hvordan:
Lese XML med SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Påminnelse</heading>
                <body>Glem ikke dette</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Utganger: Tove
echo $xml->from;     // Utganger: Jani
echo $xml->heading;  // Utganger: Påminnelse
echo $xml->body;     // Utganger: Glem ikke dette
```

Skrive XML med DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Påminnelse');
$body = $dom->createElement('body', 'Glem ikke dette');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Eksempelutskrift:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Påminnelse</heading>
  <body>Glem ikke dette</body>
</note>
```

## Dypdykk
XML, eller eXtensible Markup Language, har vært en hovedsak i data serialisering siden W3Cs anbefaling i 1998. Det er omstendelig, menneskelesbart og strengt i syntax, noe som gjør det til et pålitelig valg for konfigurasjonsfiler, datautveksling og mer. Imidlertid har det delvis blitt overskygget av JSON for web-APIer på grunn av sin enkelhet og lette natur.

Programmerere velger ofte XML når de trenger dokumentvalidering levert av XML-skjemaer eller når de arbeider innenfor økosystemer som allerede stoler tungt på det (som Microsoft Office-filformater). Håndtering av XML i PHP er greit med SimpleXML-utvidelsen for grunnleggende operasjoner. For mer kompleks manipulasjon gir DOMDocument et robust sett med funksjoner som tillater større kontroll, som navneromshåndtering og skjemavalidering.

## Se Også
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML-skjema](https://www.w3.org/XML/Schema)
