---
title:                "Substrings extraheren"
aliases:
- nl/fish-shell/extracting-substrings.md
date:                  2024-01-28T21:59:39.239154-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Substrings extraheren betekent specifieke delen uit een string halen. Programmeurs doen dit om gegevens te isoleren, input te reinigen of informatie te ontleden voor verdere verwerking.

## Hoe te:
In Fish gebruik je het `string` commando om met strings te werken. Zo doe je dat:

### Vanaf het begin pakken:
```Fish Shell
set my_string "Fish Shell is leuk!"
echo $my_string | string sub -l 4 # Geeft 'Fish'
```

### Vanaf het einde afknippen:
```Fish Shell
set my_string "Fish Shell is leuk!"
echo $my_string | string sub -s -4 # Geeft 'leuk!'
```

### Specifiek bereik:
```Fish Shell
set my_string "Fish Shell is leuk!"
echo $my_string | string sub -s 6 -l 5 # Geeft 'Shell'
```

## Diepere Duik
Vroeger sneden en hakten we strings in Fish met externe tools zoals `cut`, `awk` of `sed`. Nu is `string` onze ingebouwde functie bij uitstek, geïntroduceerd in Fish 2.3.0. Het is sneller, leesbaarder en integreert naadloos met onze scripts.

`string sub` is niet je enige optie. Andere `string` functies kunnen strings splitsen, delen vervangen of ze samenvoegen. Dit focust op minimaal gebruik van bronnen en eenvoud van begrip.

Wat implementatie betreft, wanneer je substrings extraheert, leest Fish de string en geeft alleen het deel dat je hebt gespecificeerd uit, terwijl het rekening houdt met karaktercodering en veelvoorkomende bugs in het extraheren van substrings vermijdt, zoals het doormidden splitsen van een karakter.

## Zie Ook
- Officiële Fish-documentatie over `string`: https://fishshell.com/docs/current/cmds/string.html
- Gemeenschapstutorials over Fish-scripting: https://fishshell.com/docs/current/tutorial.html
- Stack Overflow discussies over Fish string manipulatie: https://stackoverflow.com/questions/tagged/fish
