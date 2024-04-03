---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:33.716872-07:00
description: 'Hoe: Python''s `datetime` module maakt werken met datums en tijden een
  fluitje van een cent. Kijk maar.'
lastmod: '2024-03-13T22:44:50.389965-06:00'
model: gpt-4-0125-preview
summary: Python's `datetime` module maakt werken met datums en tijden een fluitje
  van een cent.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Hoe:
Python's `datetime` module maakt werken met datums en tijden een fluitje van een cent. Kijk maar:

```Python
from datetime import datetime, timedelta

# Huidige datum en tijd
now = datetime.now()
print("Nu: ", now)

# 10 dagen toevoegen
toekomstige_datum = now + timedelta(days=10)
print("Toekomstige datum (+10 dagen): ", toekomstige_datum)

# 5 dagen aftrekken
verleden_datum = now - timedelta(days=5)
print("Verleden datum (-5 dagen): ", verleden_datum)
```
Uitvoer kan er zo uitzien:
```
Nu: 2023-04-01 12:34:56.789012
Toekomstige datum (+10 dagen): 2023-04-11 12:34:56.789012
Verleden datum (-5 dagen): 2023-03-27 12:34:56.789012
```

Simpel, toch? Pas gewoon de dagen aan, of gebruik `weken`, `uren`, `minuten`, of `seconden` in `timedelta` om naar de tijd te springen die je nodig hebt.

## Diepe Duik
Heel lang geleden was het berekenen van datums en tijden een pijnlijke zaak. Je moest rekening houden met schrikkeljaren, tijdzones, zomertijd - een rommeltje. Met Python's `datetime` en zijn metgezellen `date` en `time`, is het een gladde vaart. De module handelt de complicaties achter de schermen af.

Je zou kunnen vragen naar alternatieven. Zeker. Bibliotheken zoals `dateutil` kunnen omgaan met meer complexe datummanipulaties en parsing. Het is een go-to wanneer `datetime` net niet voldoende is.

Wat betreft implementatie, wanneer je `timedelta` gebruikt, past Python de datum aan rekening houdend met schrikkeljaren en dergelijke. Controleer altijd je resultaten - vooral wanneer je te maken hebt met tijdzones. En onthoud, `datetime` is standaard naïef; het houdt geen rekening met tijdzones, tenzij je het opdraagt.

## Zie ook
- Python's `datetime` documentatie: https://docs.python.org/3/library/datetime.html
- De `dateutil` bibliotheek: https://dateutil.readthedocs.io/en/stable/
- Omgaan met tijdzones in Python: https://docs.python.org/3/library/zoneinfo.html
