---
title:                "Een string omzetten naar kleine letters"
aliases:
- /nl/python/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:05.910736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekenreeks converteren naar kleine letters betekent het veranderen van alle hoofdletters in de tekst naar hun kleine letter equivalenten. Programmeurs doen dit vaak voor consistentie, vergelijking, of zoekdoeleinden, omdat 'A' niet hetzelfde is als 'a' in de wereld van computers.

## Hoe:
Een tekenreeks naar kleine letters omzetten in Python is eenvoudig met de `.lower()` methode.
```Python
originele_tekst = "Hallo, Wereld!"
kleine_letters_tekst = originele_tekst.lower()
print(kleine_letters_tekst)  # Uitvoer: hallo, wereld!
```
Of gebruik een lijstbegrip voor meer controle:
```Python
s = "HALLO, Wereld!"
kleine_lijst = [char.lower() for char in s]
print(''.join(kleine_lijst))  # Uitvoer: hallo, wereld!
```

## Diepgaand
De `.lower()` methode is al vrij vroeg een onderdeel van Python's string type. Het is een eenvoudige manier om te zorgen voor hoofdletterongevoelige gegevensverwerking, wat nuttig is in situaties zoals hoofdletterongevoelige gebruikersinvoer.

Er zijn alternatieven, zoals het gebruik van reguliere expressies:
```Python
import re

s = "HALLO, Wereld!"
lower_s = re.sub(r'[A-Z]', lambda match: match.group(0).lower(), s)
print(lower_s)  # Uitvoer: hallo, wereld!
```
Maar dit is overdreven voor het simpelweg converteren van een tekenreeks naar kleine letters.

Onder de motorkap, vertrouwt Python's `.lower()` op Unicode tekenmapping. De Unicode standaard specificeert het kleine letter equivalent van bijna alle karakters die een hoofd-/kleine lettervariant hebben. Dit proces is complexer dan gewoon een waarde aftrekken om van 'A' naar 'a' te gaan, omdat niet alle talen en schriften zo'n eenvoudige en directe mapping hebben.

## Zie Ook
- De Python documentatie over string methoden: https://docs.python.org/3/library/stdtypes.html#string-methods
- Details over Unicode hoofd-/kleine letter mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- Een tutorial over Python lijstbegrippen: https://realpython.com/list-comprehension-python/
