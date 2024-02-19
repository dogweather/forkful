---
aliases:
- /nl/haskell/parsing-html/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:40.394195-07:00
description: "HTML parsen betekent gegevens uit HTML-documenten extraheren - HTML\
  \ is tenslotte de steiger van het web. Programmeurs parsen HTML om gegevensschrapen\
  \ te\u2026"
lastmod: 2024-02-18 23:09:01.896361
model: gpt-4-0125-preview
summary: "HTML parsen betekent gegevens uit HTML-documenten extraheren - HTML is tenslotte\
  \ de steiger van het web. Programmeurs parsen HTML om gegevensschrapen te\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen betekent gegevens uit HTML-documenten extraheren - HTML is tenslotte de steiger van het web. Programmeurs parsen HTML om gegevensschrapen te automatiseren, om inhoud te migreren, of om het om te zetten naar verschillende formaten.

## Hoe:

Laten we onze handen vuil maken met wat code, door de `tagsoup` bibliotheek te gebruiken om een eenvoudig HTML-snippet te parsen. Zorg er eerst voor dat je het pakket van Hackage installeert via `cabal install tagsoup`.

```Haskell
import Text.HTML.TagSoup

-- Laten we een eenvoudig HTML-snippet parsen
let html = "<html><body><p>Hallo, Haskell!</p></body></html>"

-- Parse het
let parsedHtml = parseTags html

-- Zoek naar paragrafen
let paragraphs = partitions (~== "<p>") parsedHtml

-- Haal de tekst uit de eerste paragraaf
let firstParagraphText = innerText $ head paragraphs

-- Voila!
print firstParagraphText
```

Voorbeelduitvoer:
```
"Hallo, Haskell!"
```
Dit snippet parseert een HTML-string, zoekt naar paragraaftags en print de tekst die in de eerste paragraaf staat. Netjes en simpel.

## Diepere Duik

HTML parsen in Haskell is niet altijd zo gestroomlijnd geweest als vandaag de dag. Er was eens een tijd dat mensen hun eigen parsers maakten of worstelden met bibliotheken op een lager niveau, HTML parsend alsof het het Wilde Westen was.

Tegenwoordig heb je opties. `tagsoup`, zoals we hebben gebruikt, is geweldig wanneer de HTML-structuur meer suggestie dan regel is - het is tolerant voor rommelige HTML uit de echte wereld. Als je op zoek bent naar meer nauwkeurigheid, kunnen `html-conduit` gecombineerd met `xml-conduit` uit het `conduit` pakket jouw ding zijn. Ze gebruiken een streamingbenadering en zijn kieskeuriger over structuur.

Onder de motorkap zetten deze bibliotheken HTML om in een boom of een soep van tags. Ze bieden handige functies om deze gegevens te bevragen en te manipuleren, waardoor HTML-parsing minder hoofdpijn geeft. Beschouw ze als een schatkaart, waarbij X de paragraaftag markeert.

## Zie Ook

- [`tagsoup` op Hackage](https://hackage.haskell.org/package/tagsoup)
- [`html-conduit` op Hackage](https://hackage.haskell.org/package/html-conduit)
- [Beautiful Soup documentatie](https://www.crummy.com/software/BeautifulSoup/) - Hoewel niet Haskell, heeft de aanpak van Beautiful Soup ten aanzien van 'tagsoup' vergelijkbare bibliotheken in de Haskell-wereld beïnvloed.
- [XPath en XQuery Functies en Operatoren op W3C](https://www.w3.org/TR/xpath-functions/) - Diep duiken in normen kan informeren over de structuur en bevraging van XML/HTML-documenten, nuttig voor het begrijpen van de parseringsstrategieën op de achtergrond.
