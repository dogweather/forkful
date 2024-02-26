---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:08.816746-07:00
description: "Zoeken en vervangen van tekst gaat over het vinden van tekenreeksen\
  \ in een tekstblok en deze veranderen in iets anders. Programmeurs doen dit voor\
  \ het\u2026"
lastmod: '2024-02-25T18:49:47.755266-07:00'
model: gpt-4-0125-preview
summary: "Zoeken en vervangen van tekst gaat over het vinden van tekenreeksen in een\
  \ tekstblok en deze veranderen in iets anders. Programmeurs doen dit voor het\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?
Zoeken en vervangen van tekst gaat over het vinden van tekenreeksen in een tekstblok en deze veranderen in iets anders. Programmeurs doen dit voor het bewerken van code, het verwerken van gegevens of het automatiseren van refactor-taken.

## Hoe:
```Python
# Gebruik van str.replace() voor eenvoudige vervanging
tekst = "Ik vind Python leuk. Python is geweldig!"
tekst = tekst.replace("Python", "programmeren")
print(tekst)  # Uitvoer: Ik vind programmeren leuk. programmeren is geweldig!

# Gebruik van re.sub() voor patroongebaseerde vervanging met regex
import re
tekst = "Neem contact met ons op via support@voorbeeld.com"
nieuwe_tekst = re.sub(r'\b[a-zA-Z0-9.-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\b', 'support@nieuwdomein.com', tekst)
print(nieuwe_tekst)  # Uitvoer: Neem contact met ons op via support@nieuwdomein.com
```

## Diepere Duik
In de vroege dagen van programmeren was tekstbewerking een handmatige sleur. Toen kwamen regex (reguliere expressies), gebouwd in de jaren 1950, waardoor zoeken een minder hoofdpijnverwekkende aangelegenheid werd. Voor eenvoudige vervangingen is `str.replace()` je beste optie. Het is eenvoudig en geweldig voor eenmalige vervangingen. Wanneer je te maken hebt met patronen, zoals telefoonnummers, e-mails of datums, is regex met `re.sub()` de toverstaf. Het vindt patronen met een speciale syntaxis en wisselt ze uit. Houd er rekening mee dat regex net zo grillig kan zijn als het krachtig is; het is een hulpmiddel waarbij je beter wordt naarmate je meer puzzels oplost.

## Zie Ook
- [Python `str.replace()` documentatie](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Python `re` module documentatie](https://docs.python.org/3/library/re.html)
- [Regex101](https://regex101.com/): Om regex-patronen online te testen
- [Automate the Boring Stuff with Python](https://automatetheboringstuff.com/): Een boek waar je meer kunt leren over praktische tekstverwerkingstaken.
