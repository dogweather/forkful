---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:38.280905-07:00
description: "Een tekstbestand schrijven in Python betekent het opslaan van strings\
  \ in een bestand op je schijf. Programmeurs doen dit om gegevens tussen sessies\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.394719-06:00'
model: gpt-4-0125-preview
summary: Een tekstbestand schrijven in Python betekent het opslaan van strings in
  een bestand op je schijf.
title: Een tekstbestand schrijven
weight: 24
---

## Wat & Waarom?

Een tekstbestand schrijven in Python betekent het opslaan van strings in een bestand op je schijf. Programmeurs doen dit om gegevens tussen sessies te bewaren, informatie te loggen of leesbare resultaten te exporteren.

## Hoe:

Schrijven naar een bestand is eenvoudig. Gebruik de `with` instructie om een bestand te openen en roep vervolgens `write()` aan.

```Python
# Schrijven naar een bestand in Python
with open('voorbeeld.txt', 'w') als bestand:
    bestand.write("Hallo, Wereld!")

# Het bestand teruglezen
with open('voorbeeld.txt', 'r') als bestand:
    print(bestand.read())
```

Voorbeelduitvoer:
```
Hallo, Wereld!
```

Toevoegen aan een bestaand bestand zonder overschrijven:

```Python
# Toevoegen aan een bestand in Python
with open('voorbeeld.txt', 'a') als bestand:
    bestand.write("\nTot ziens, Wereld!")

# Het aangevulde bestand lezen
with open('voorbeeld.txt', 'r') als bestand:
    print(bestand.read())
```

Voorbeelduitvoer:
```
Hallo, Wereld!
Tot ziens, Wereld!
```

## Diepgaande Duik

Het schrijven van tekstbestanden heeft zijn wortels in vroege computersystemen. Het is de meest basale vorm van gegevensbehoud en -uitwisseling tussen programma's en systemen. Hoewel er alternatieven zoals databases bestaan voor complexe gegevens, worden tekstbestanden veel gebruikt vanwege hun eenvoud en menselijke leesbaarheid. Bij het schrijven van bestanden, behandelt Python vele complexiteiten, zoals buffering en geheugenbeheer, achter de schermen en biedt verschillende modi (bijv. schrijven 'w', toevoegen 'a') voor verschillende gebruiksgevallen.

## Zie Ook

- Python's officiële documentatie over bestand I/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Real Python handleiding over bestand I/O: https://realpython.com/read-write-files-python/
- Verder lezen over bestandsbehandling in Python met contextbeheerders: https://docs.python.org/3/reference/compound_stmts.html#the-with-statement
