---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:56.155272-07:00
description: "Hoe: Fish Shell heeft geen ingebouwde datum parsing functies zoals sommige\
  \ andere talen. In plaats daarvan leunt het op externe hulpmiddelen zoals `date`.\u2026"
lastmod: '2024-04-05T21:53:51.264763-06:00'
model: gpt-4-0125-preview
summary: Fish Shell heeft geen ingebouwde datum parsing functies zoals sommige andere
  talen.
title: Een datum uit een string parsen
weight: 30
---

## Hoe:
```Fish Shell
# Basisdatum parsing met behulp van de `strptime` functie
set date_string "2023-04-15"
set -l formaat "%Y-%m-%d"
set -l geparseerde_datum (string tolower (date -u --date=$date_string +"$formaat"))

echo $geparseerde_datum # Geeft uit: 2023-04-15
```

```Fish Shell
# Omgaan met meerdere datumformaten met een switch
set date_string1 "15-04-2023"
set date_string2 "April 15, 2023"

function parse_date -a date_string
    switch $date_string
        case "*-*-*"
            date --date=$date_string +%Y-%m-%d
        case "* *, *"
            date --date=$date_string +%Y-%m-%d
    end
end

echo (parse_date $date_string1) # Geeft uit: 2023-04-15
echo (parse_date $date_string2) # Geeft uit: 2023-04-15
```

## Diepere Duik
Fish Shell heeft geen ingebouwde datum parsing functies zoals sommige andere talen. In plaats daarvan leunt het op externe hulpmiddelen zoals `date`. Het `date` commando is veelzijdig en met hulp van `strptime` (string parse time), dat een standaard C-bibliotheek functie is, kan het veel datumformaten aan.

Voor `date` en `strptime` schreven programmeurs aangepaste parsers—vaak buggy en complex. Nu handelen hulpprogramma's de eigenaardigheden van tijdzones en schrikkeljaren af, wat ons hoofdpijn bespaart.

Alternatieven? Zeker, scripttalen zoals Python hebben robuuste datumtijdbibliotheken zoals `datetime`. Maar Fish, als 'shell', verkiest lichtgewicht, command-line programma's voor een klus als deze.

In onze voorbeelden gebruikten we `switch` om het datumformaat voor `date` te kiezen om te parsen. Het is schoon en uitbreidbaar. Wil je meer formaten? Voeg meer `case` blokken toe.

Waarom `string tolower` in het eerste voorbeeld? Het gaat om consistentie, om te zorgen dat de formaatreeks en uitvoer uniform in kleine letters zijn. Een klein gebaar, maar het illustreert de voorkeur van Fish voor eenvoudige stringbewerkingen.

## Zie Ook
- De `date` manpagina: `man date`
- Documentatie voor stringmanipulatie in Fish Shell: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Algemene gebruiksvormen van het datumcommando: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
