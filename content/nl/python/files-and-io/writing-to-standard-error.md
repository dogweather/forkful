---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:31.325356-07:00
description: "Standaardfout, vaak aangeduid als stderr, is een vooraf gedefinieerde\
  \ bestandsstroom voor het loggen van foutmeldingen. Programmeurs gebruiken het om\u2026"
lastmod: '2024-03-13T22:44:50.392832-06:00'
model: gpt-4-0125-preview
summary: Standaardfout, vaak aangeduid als stderr, is een vooraf gedefinieerde bestandsstroom
  voor het loggen van foutmeldingen.
title: Schrijven naar standaardfout
weight: 25
---

## Wat & Waarom?
Standaardfout, vaak aangeduid als stderr, is een vooraf gedefinieerde bestandsstroom voor het loggen van foutmeldingen. Programmeurs gebruiken het om reguliere programma-uitvoer te scheiden van foutmeldingen, wat het debuggen gemakkelijker maakt.

## Hoe te:
Om naar stderr in Python te schrijven:

```Python
import sys

print("Dit is een normaal bericht.")
sys.stderr.write("Dit is een foutmelding.\n")
```

Voorbeelduitvoer:
```
Dit is een normaal bericht.
Dit is een foutmelding.
```

Merk op dat terwijl `print()` standaard een nieuwe regel toevoegt, `sys.stderr.write()` dat niet doet - je moet `\n` toevoegen om een nieuwe regel te beginnen.

## Diepgaande verkenning
Historisch gezien werden standaardstromen geïntroduceerd in Unix. Er zijn er drie: standaardinvoer (`stdin`), standaarduitvoer (`stdout`) en standaardfout (`stderr`). In Python biedt de `sys`-module toegang tot deze stromen. Terwijl `stdout` typisch wordt gebruikt voor de hoofduitvoer van een programma, is `stderr` gereserveerd voor foutmeldingen en diagnostiek.

Alternatieven voor `sys.stderr.write()` omvatten het gebruik van `print()` met het `file`-argument:

```Python
print("Dit is een foutmelding.", file=sys.stderr)
```

Dit presteert vergelijkbaar, maar maakt gebruik van de gebruiksvriendelijke functies van `print()`. Wat interne mechanica betreft, maken beide methoden uiteindelijk systeemniveau schrijfoproepen naar de respectievelijke stroom.

## Zie ook
- Python-documentatie voor de sys-module: https://docs.python.org/3/library/sys.html
- Unix Standaardstromen: https://nl.wikipedia.org/wiki/Standaardstromen
- Discussie over het gebruik van stderr: https://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python
