---
aliases:
- /nl/python/working-with-csv/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:19.938921-07:00
description: "Werken met CSV (Comma-Separated Values) bestanden betekent het lezen\
  \ van en schrijven naar platte tekstbestanden waarbij elke rij een datagegeven is.\u2026"
lastmod: 2024-02-18 23:09:01.457928
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma-Separated Values) bestanden betekent het lezen van\
  \ en schrijven naar platte tekstbestanden waarbij elke rij een datagegeven is.\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met CSV (Comma-Separated Values) bestanden betekent het lezen van en schrijven naar platte tekstbestanden waarbij elke rij een datagegeven is. Programmeurs zijn dol op CSV's omdat ze licht, leesbaar voor mensen en compatibel met vrijwel elk gegevensverwerkingshulpmiddel zijn.

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
