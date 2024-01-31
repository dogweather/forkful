---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV hantering handlar om att läsa och skriva data i komma-separerade värden, ett enkelt textbaserat format. Programmerare använder CSV för att enkelt utbyta data mellan olika system och program.

## How to:
### Läs in CSV:
```Python
import csv

# Öppna och läs CSV-fil
with open('exempel.csv', mode='r') as file:
    reader = csv.reader(file)
    for rad in reader:
        print(rad)
```

### Skriv till CSV:
```Python
import csv

# Data att skriva
data = [['Namn', 'Ålder'], ['Alice', 30], ['Bob', 25]]

# Öppna och skriv till CSV-fil
with open('exempel.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)
```

### Resultat:
```
['Namn', 'Ålder']
['Alice', '30']
['Bob', '25']
```

## Deep Dive
CSV (Comma-Separated Values) har använts sedan 1970-talet och är fortfarande populärt på grund av dess enkelhet. Alternativa format inkluderar JSON och XML, men CSV är ofta överlägset när det gäller enkelhet och små filstorlekar. Python´s `csv` modul hanterar kompatibilitet väl, men håll koll på skillnader som kan uppstå med textkodning och radavslutningar beroende på operativsystem.

## See Also
- Officiell Python `csv` modul dokumentation: https://docs.python.org/3/library/csv.html
- Mer om CSV-formatet: https://en.wikipedia.org/wiki/Comma-separated_values
- Pandas biblioteket för avancerad dataanalys: https://pandas.pydata.org/
