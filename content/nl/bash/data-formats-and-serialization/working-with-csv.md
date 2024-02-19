---
aliases:
- /nl/bash/working-with-csv/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:34.292090-07:00
description: "Werken met CSV, wat staat voor \"Comma-Separated Values\" (komma-gescheiden\
  \ waarden), omvat het parsen en manipuleren van gegevens in een tabulair\u2026"
lastmod: 2024-02-18 23:09:02.067181
model: gpt-4-0125-preview
summary: "Werken met CSV, wat staat voor \"Comma-Separated Values\" (komma-gescheiden\
  \ waarden), omvat het parsen en manipuleren van gegevens in een tabulair\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV, wat staat voor "Comma-Separated Values" (komma-gescheiden waarden), omvat het parsen en manipuleren van gegevens in een tabulair tekstformaat. Programmeurs doen dit omdat CSV een gangbaar, eenvoudig bestandsformaat is dat wordt gebruikt voor de uitwisseling van gegevens tussen verschillende applicaties en systemen.

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
