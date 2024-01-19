---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sletting av tegn som matcher et mønster er en prosess hvor spesifikke karakterer fjernes fra en tekststreng basert på et definert mønster. Dette gjøres av programmerere for å manipulere tekst data, og for å forbedre datarensing og tekstbehandling.

## Hvordan:

Her er et eksempel på bruk av Python's innebygde "re" modul for å slette alle ikke-tall fra en tekststreng. 

```Python
import re

def delete_pattern(input_str, pattern):
    return re.sub(pattern, '', input_str)

input_str = "123abc456def"
pattern = "[^0-9]"  # alt som ikke er et tall
print(delete_pattern(input_str, pattern))  # output: 123456
```
Erstatt "pattern" med det mønsteret du ønsker å slette, f.eks. "[^a-zA-Z]" for å slette alt som ikke er en bokstav.

## Deep Dive:

Historisk sett, konseptet av mønster matching kommer fra formelle språkteorier og ble først implementert i programmeringsspråk gjennom bruk av regulære uttrykk (regular expressions). I Python, er den mest direkte måten å slette tegn som matcher et mønster å bruke Python's "re" modul.

Det finnes også alternative måter å oppnå dette på. En kan bruke list comprehension, eller "filter"- og "translate"-funksjonen. Men i forhold til ytelse og lesbarhet, er "re.sub()" generelt det beste valget.

Implementeringen av "re.sub()" i Python er basert på Thompson's algoritme for konstruksjon av en ikke-deterministisk endelige automaten (NFA) fra et regulært uttrykk.

## Se også:

- Python's "re" modul dokumentasjon: https://docs.python.org/3/library/re.html
- Introduksjon til Regulære Uttrykk i Python: https://realpython.com/regex-python/
- Python's 'translate' og 'filter' funksjoners dokumentasjon: https://docs.python.org/3/library/stdtypes.html#str.translate, https://docs.python.org/3/library/functions.html#filter