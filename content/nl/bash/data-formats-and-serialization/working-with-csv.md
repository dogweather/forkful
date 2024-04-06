---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:34.292090-07:00
description: ''
lastmod: '2024-04-05T22:51:03.811504-06:00'
model: gpt-4-0125-preview
summary: ''
title: Werken met CSV
weight: 37
---

## Hoe te:


### Lezen uit een CSV-bestand:
```Bash
while IFS=, read -r col1 col2 col3
do
  echo "Kolom 1: $col1 | Kolom 2: $col2 | Kolom 3: $col3"
done < myfile.csv
```

Voorbeelduitvoer:

```
Kolom 1: data1 | Kolom 2: data2 | Kolom 3: data3
```

### Schrijven naar een CSV-bestand:
```Bash
echo "data1,data2,data3" > myfile.csv
```

### Toevoegen aan een CSV-bestand:
```Bash
echo "data4,data5,data6" >> myfile.csv
```

## Diepgaande Verkenning
Het CSV-formaat heeft zijn wortels in de vroege computertechnologie en is een pijler geworden in de gegevensuitwisseling omdat het wordt ondersteund door een breed scala aan software. Hoewel Bash CSV-bestanden kan verwerken, is het niet uitgerust voor complexe parsing. Alternatieven voor meer ingewikkelde taken omvatten AWK, Sed, of het gebruik van een volledige programmeertaal zoals Python. Implementatiedetails om te overwegen bij het werken met CSV in Bash omvatten het omgaan met speciale karakters, complexe citaties en regelonderbrekingen binnen velden.

## Zie Ook
- [GNU Coreutils Documentatie](https://www.gnu.org/software/coreutils/)
- [Bash Referentiehandleiding](https://www.gnu.org/software/bash/manual/)
- [Inleiding tot AWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Sed bijvoorbeeld](https://www.gnu.org/software/sed/manual/sed.html)

Voor meer geavanceerde CSV-manipulatie:
- [Python CSV Module Doc](https://docs.python.org/3/library/csv.html)
- [Pandas Bibliotheek voor Python](https://pandas.pydata.org/)
