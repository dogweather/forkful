---
title:                "Werken met CSV"
aliases: - /nl/python/working-with-csv.md
date:                  2024-01-28T22:10:19.938921-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
