---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:19.938921-07:00
description: "Hoe te: Terugkijkend op de tijd dat de gegevensoverdracht langzamer\
  \ was en opslag duurder, won CSV aanhangers vanwege zijn eenvoud en lage overhead.\u2026"
lastmod: '2024-04-05T21:53:50.423919-06:00'
model: gpt-4-0125-preview
summary: Terugkijkend op de tijd dat de gegevensoverdracht langzamer was en opslag
  duurder, won CSV aanhangers vanwege zijn eenvoud en lage overhead.
title: Werken met CSV
weight: 37
---

## Hoe te:
```python
# Importeer de CSV-module
import csv

# Een CSV-bestand lezen
with open('data.csv', 'r') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Uitvoer:
# ['Naam', 'Leeftijd', 'Stad']
# ['Alice', '30', 'New York']
# ...

# Schrijven naar een CSV-bestand
with open('output.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['Naam', 'Leeftijd', 'Stad'])
    writer.writerow(['Bob', '22', 'Los Angeles'])

# Controleer output.csv om de resultaten te zien
```

## Diepgaand
Terugkijkend op de tijd dat de gegevensoverdracht langzamer was en opslag duurder, won CSV aanhangers vanwege zijn eenvoud en lage overhead. Alternatieven zoals JSON en XML bieden structuur, maar tegen de kosten van langdradigheid. Voor CSV is de snelheid van het parsen een voordeel, maar het kan moeilijk zijn met complexe hiërarchieën of datatypes.

Bibliotheken zoals `pandas` kunnen ook CSV's aan, en bieden meer kracht maar vereisen meer middelen. Onder de motorkap is csv.reader() een generator, die rijen één voor één levert - slim voor geheugenbeheer.

## Zie ook
- Documentatie voor het lezen/schrijven van CSV's in Python: https://docs.python.org/3/library/csv.html
- `pandas` bibliotheek voor complexe gegevensverwerking: https://pandas.pydata.org/
- CSV vs. JSON vs. XML: Een vergelijking van gegevensformaten: https://www.datacamp.com/community/tutorials/json-xml-csv
