---
title:                "Tekst zoeken en vervangen"
date:                  2024-01-28T22:07:05.417146-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst zoeken en vervangen in bash betekent het omwisselen van woorden of patronen in een string of bestand met iets anders. Het is een dagelijkse taak voor het opschonen van gegevens, het repareren van code, of het automatiseren van bewerkingen.

## Hoe te:
Hier is hoe je de kracht van zoeken en vervangen in bash gebruikt:

1. Tekst binnen een string verwisselen met `sed`:
```Bash
echo "Hello world" | sed 's/world/universe/'
# Uitvoer: Hello universe
```

2. Tekst in een bestand vervangen, met het opslaan van de wijzigingen:
```Bash
sed -i 's/old_text/new_text/g' file.txt
```

3. Variabelen gebruiken in je zoek- en vervangopdracht:
```Bash
old="apple"
new="banana"
sed "s/$old/$new/g" <<< "I like apple pies"
# Uitvoer: I like banana pies
```

Onthoud, `g` aan het einde betekent "globaal", dus je verandert elke overeenkomst in de regel, niet slechts de eerste.

## Diepgaand

We hebben al eeuwen hulpprogramma's voor tekstverwerking op Unix-achtige systemen. `sed`, kort voor Stream Editor, is zo'n hulpprogramma en het bestaat al sinds de jaren 1970. Het is niet alleen voor eenvoudige vervangingen; `sed` kan ook tekst in complexe patronen snijden en hakken.

Alternatieven? Zeker. `awk` is een beetje geavanceerder en kan wonderen verrichten met kolommen en rijen. Voor snelle oplossingen kan `grep` je helpen dingen te vinden, maar het zal niet vervangen - het is meer als de uitkijk.

Onder de motorkap gebruikt `sed` reguliere expressies, die zijn als wildcards op steroïden. Ze kunnen bijna elk patroon matchen waar je aan kunt denken. Het maakt `sed` ongelooflijk krachtig, maar ook een beetje lastig om te beheersen.

## Zie Ook
- `man sed` voor de handleiding over `sed`
- [Een introductie tot `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- De Kunst van de Command Line voor meer bash trucjes (https://github.com/jlevy/the-art-of-command-line)
