---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:09:15.121981-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die zoekcriteria voor tekst definiëren. Programmeurs gebruiken ze voor het matchen, vervangen of extraheren van delen van strings op basis van deze patronen - denk aan complex zoeken-en-vervangen op steroïden.

## Hoe te:
```Bash
# Een patroon matchen
echo "I love to code in Bash" | grep -oP 'code'

# Uitvoer:
code

# String vervangen met regex met sed
echo "Bash 2023" | sed -E 's/[0-9]+/2024/'

# Uitvoer:
Bash 2024

# Substring extraheren met regex
echo "Error: Line 42" | grep -oP '(?<=Line )\d+'

# Uitvoer:
42
```

## Diepere Duik
Reguliere expressies bestaan al sinds de jaren 1950, oorspronkelijk bedacht door wiskundige Stephen Kleene. Alternatieven voor Bash regex zijn het gebruik van `awk` of `perl`, die hun eigen regex-mogelijkheden hebben. Uitvoeringsgewijs gebruikt Bash grep voor matching, `sed` voor vinden-en-vervangen, en de `=~` operator binnen `[[ ]]` voor conditionals. Wees je bewust dat regex kan variëren tussen tools (`grep`, `egrep`, `sed`, en `awk`), dus ken de smaak waarmee je werkt.

## Zie Ook
- [GNU Grep Handleiding](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed - Een Introductie en Tutorial](https://www.grymoire.com/Unix/Sed.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex101: Online Regex Tester en Debugger](https://regex101.com/)
