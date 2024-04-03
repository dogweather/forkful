---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:20.368435-07:00
description: 'Hoe te: .'
lastmod: '2024-03-13T22:44:50.367386-06:00'
model: gpt-4-0125-preview
summary: .
title: De lengte van een string vinden
weight: 7
---

## Hoe te:
```python
# Eenvoudig gebruik van de len() functie
mijn_string = "Hallo, Wereld!"
lengte = len(mijn_string)
print(lengte)  # Uitvoer: 13

# Lengte in een lus
for i in range(len(mijn_string)):
    print(mijn_string[i], end='')  # Uitvoer: Hallo, Wereld!
print()  # Voor nieuwe regel

# Stringlengte combineren met andere bewerkingen
if len(mijn_string) > 10:
    print("Het is een lange string!")  # Uitvoer: Het is een lange string!
```

## Diepgaand
Historisch gezien is de `len()` functie Python's standaard manier om de lengte van een string te vinden. Het is elegant en snel. Onderliggend zijn Python strings arrays van bytes die Unicode karakters voorstellen, en `len()` telt deze. De functie werkt niet alleen met strings, maar met elk iterabel.

Alternatieven? Wel, niet vaak gebruikt voor strings, maar je zou handmatig door een string kunnen lopen en karakters kunnen tellen—onhandig en inefficiënt. Voor Unicode-ondersteuning was de lengte van een string soms anders dan de geheugengrootte, maar aangezien Python 3's strings Unicode-native zijn, vertegenwoordigt de `len()` nauwkeurig het aantal karakters.

Implementatiegewijs zijn Python-strings objecten met metadata, inclusief lengte, dus `len()` is eigenlijk een O(1) operatie—constante tijd, ongeacht de grootte van de string. Dat is alsof je met je vingers knipt en een antwoord krijgt.

## Zie Ook
- Python documentatie voor `len()`: https://docs.python.org/3/library/functions.html#len
- Unicode en Stringcodering in Python: https://docs.python.org/3/howto/unicode.html
- Python's tijdcomplexiteit voor ingebouwde typen: https://wiki.python.org/moin/TimeComplexity
