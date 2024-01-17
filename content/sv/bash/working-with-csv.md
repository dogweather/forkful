---
title:                "Arbeta med csv"
html_title:           "Bash: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med CSV är en vanlig uppgift för programmerare. CSV står för "Comma Separated Values" och är ett sätt att lagra och strukturera stora mängder data i en enkel och textbaserad format. Det används ofta för att läsa och skriva data mellan olika program och verktyg.

## Hur man gör:
Så här kan man arbeta med CSV i Bash:

```Bash
#Läs in data från en CSV-fil och skriv ut den på skärmen
while read -r line; do
    echo $line
done < file.csv

#Lägg till en ny rad i en befintlig CSV-fil
echo "John,Smith,30" >> file.csv

#Spara resultatet av ett kommando till en CSV-fil 
ls -l > file.csv

#Sök efter en specifik rad i en CSV-fil
grep "Sweden" file.csv
```

## Djupdykning:
CSV-formatet uppfanns på 1970-talet som ett sätt att enkelt överföra data mellan olika databaser. Idag är det ett vanligt format för att dela och föra över data mellan olika program, speciellt inom områden som datavetenskap och statistik. Det finns flera alternativ för att arbeta med CSV i Bash, till exempel "csvtool" och "awk". 

## Se även:
För mer information och tips på hur man kan arbeta med CSV i Bash, besök följande länkar:
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/)
- [csvtool documentation](https://github.com/Chris00/ocaml-csv/blob/master/src/csvtool/csvtool.1)
- [awk tutorial](https://www.gnu.org/software/gawk/manual/gawk.html)