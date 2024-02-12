---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:10:06.534563-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die gebruikt worden om combinaties van karakters in strings te vinden. Programmeurs gebruiken regex voor het zoeken, bewerken of manipuleren van tekst omdat het krachtig en efficiënt is.

## Hoe:
Hieronder staan Python voorbeelden die het `re` module gebruiken voor gangbare regex operaties:

```Python
import re

# Vind alle overeenkomsten van 'abc' in een string
matches = re.findall('abc', 'abc123abc')
print(matches)  # Uitvoer: ['abc', 'abc']

# Zoek naar 'def' en retourneer een Match object
match = re.search('def', '123def456')
if match:
    print(match.group())  # Uitvoer: 'def'

# Vervang 'ghi' door 'xyz'
replaced = re.sub('ghi', 'xyz', 'ghi123ghi')
print(replaced)  # Uitvoer: 'xyz123xyz'
```

## Diepgaande duik
Reguliere expressies bestaan al sinds de jaren 1950, ontwikkeld samen met de theorie van formele talen. Alternatieven voor regex omvatten parsing bibliotheken en string methoden zoals `str.find()` of `str.replace()`, maar deze missen de patroon-matchende veelzijdigheid van regex. Wat de implementatie betreft, gebruikt Python het `re` module, dat gebaseerd is op de traditionele UNIX regex bibliotheek maar inclusief enkele verbeteringen.

## Zie ook
- Python `re` module documentatie: https://docs.python.org/3/library/re.html
- Gids voor reguliere expressie syntax: https://www.regular-expressions.info/
- Regex tester en debugger: https://regex101.com/
