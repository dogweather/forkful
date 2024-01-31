---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jobba med CSV (Comma-Separated Values) handlar om att hantera data separerade med kommatecken. Programmerare gör det för att enkelt läsa, skapa och manipulera data i en strukturerad och lättöverskådlig textformat.

## How to:
```Bash
# Läs innehåll från en CSV-fil
while IFS=, read -r col1 col2 col3; do
  echo "Kolumn 1: $col1 - Kolumn 2: $col2 - Kolumn 3: $col3"
done < fil.csv

# Sortera CSV efter en kolumn
sort -t, -k2,2 fil.csv

# Omvandla CSV till JSON
awk -F, '{ printf "{\"kolumn1\":\"%s\",\"kolumn2\":\"%s\",\"kolumn3\":\"%s\"}\n", $1, $2, $3 }' fil.csv
```
```Terminal
Kolumn 1: data1 - Kolumn 2: data2 - Kolumn 3: data3
Kolumn 1: annan_data1 - Kolumn 2: annan_data2 - Kolumn 3: annan_data3
```

## Deep Dive:
CSV-formatet har använts sedan 1970-talet för att byta data mellan program och system. Alternativ till CSV inkluderar JSON, XML och olika binära format. När man jobbar med CSV-filer i Bash är det viktigt att hantera olika typer av delimiter, escape-tecken och specialfall där data innehåller kommatecken.

## See Also:
- [GNU Coreutils: sort](https://www.gnu.org/software/coreutils/manual/html_node/sort-invocation.html)
- [AWK User's Guide - GAWK](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
