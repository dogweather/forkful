---
title:                "Analyse av HTML"
date:                  2024-01-20T15:33:02.577964-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML betyr å lese og tolke HTML-koden for å forstå strukturen og innholdet. Programmerere parser HTML for å manipulere, hente ut data, eller integrere med andre tjenester.

## Hvordan:
For å parse HTML i PHP kan du bruke `DOMDocument` klassen. Her er et enkelt eksempel:

```PHP
<?php
// Innholdet du vil parse
$htmlString = '<!DOCTYPE html><html><body><h1>Hei Norge</h1></body></html>';

// Opprett en ny DOMDocument
$dom = new DOMDocument();

// Last inn HTML, undertrykk feilmelding med @
@$dom->loadHTML($htmlString);

// Finn h1-tag ved hjelp av DOMXPath
$xpath = new DOMXPath($dom);
$h1 = $xpath->query('//h1')->item(0);

// Skriv ut h1-innholdet
echo $h1->nodeValue;
?>
```

Utdata vil bli `Hei Norge`.

## Dypdykk:
Historisk har HTML-parsing vært plundrete på grunn av ulike nettleserstandarder. `DOMDocument` og `DOMXPath` i PHP gjør det enklere. Disse er foretrukne fremfor regulære uttrykk på grunn av nøyaktighet og mindre sjanse for feil. Vær oppmerksom på `loadHTML` kan være kresen med HTML5, så `libxml_use_internal_errors(true);` kan være nyttig for å undertrykke parse-feil. Andre verktøy inkluderer SimpleXML og tredjeparts biblioteker som `phpQuery`.

## Se Også:
- Offisiell PHP dokumentasjon for DOMDocument: https://www.php.net/manual/en/class.domdocument.php
- SimpleXML funksjonalitet: https://www.php.net/manual/en/book.simplexml.php
- `phpQuery` på GitHub: https://github.com/punkave/phpQuery
- Andre biblioteker på Packagist: https://packagist.org/