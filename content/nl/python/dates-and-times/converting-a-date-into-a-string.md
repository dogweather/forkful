---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:25.041199-07:00
description: "Datums omzetten naar strings verandert een datumobject in een tekstformaat.\
  \ Programmeurs doen dit om datums op een gebruikersvriendelijke manier weer te\u2026"
lastmod: '2024-03-13T22:44:50.388048-06:00'
model: gpt-4-0125-preview
summary: Datums omzetten naar strings verandert een datumobject in een tekstformaat.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
Python maakt het eenvoudig om datums naar strings om te zetten. Gebruik de `strftime` methode die beschikbaar is op datumobjecten. Zo werkt het:

```Python
from datetime import datetime

# Haal de huidige datum en tijd op
nu = datetime.now()

# Zet het om naar een string in het formaat: Maand dag, Jaar
datum_string = nu.strftime("%B %d, %Y")
print(datum_string)  # Uitvoer: Maart 29, 2023 (of huidige datum)

# Formaat: JJJJ-MM-DD
iso_datum_string = nu.strftime("%Y-%m-%d")
print(iso_datum_string)  # Uitvoer: 2023-03-29 (of huidige datum)
```

## Diepere Duik
Historisch gezien is de omzetting van datum naar string een basis in het programmeren vanwege de noodzaak om datums weer te geven in een voor mensen leesbaar formaat.

Alternatieven voor `strftime` omvatten het gebruik van de `isoformat` methode voor het ISO 8601 formaat, of externe bibliotheken zoals `arrow` en `dateutil` die meer flexibele parsing- en formatteringsopties bieden.

Implementatie-wise staat `strftime` voor "string format time" en heeft het zijn wortels in de C-programmering. Python's `strftime` interpreteert opmaakcodes zoals `%Y` voor het jaar en `%m` voor de maand, wat zorgt voor bijna eindeloze aanpasbaarheid.

## Zie Ook
Om dieper in te gaan op Python's datum- en tijd functies:
- Python's officiële `datetime` documentatie: https://docs.python.org/3/library/datetime.html
- Voor degenen die geïnteresseerd zijn in een uitgebreide lijst van `strftime` richtlijnen: https://strftime.org/
- Om externe datum/tijd bibliotheken te verkennen:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
