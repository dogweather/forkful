---
title:                "Jobbe med CSV"
html_title:           "Python: Jobbe med CSV"
simple_title:         "Jobbe med CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du jobber med dataanalyse eller data science, er det stor sjanse for at du vil komme over CSV-filer. CSV (Comma Separated Values) er en vanlig filtype for å lagre tabellformet data i en enkel og lesbar tekstformat. Ved å lære å jobbe med CSV-filer, vil du kunne importere og eksportere data i en rekke programmeringsspråk, inkludert Python.

## Slik gjør du det
Å jobbe med CSV-filer i Python er enkelt og krever bare noen få linjer med kode. Først må du importere "csv" biblioteket ved å skrive: 
```Python
import csv 
``` 
Nå kan du åpne en CSV-fil ved å bruke "with" statement og lese filen med "csv.reader()":
```Python
with open('data.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        print(row)
```
Dette vil skrive ut hver rad i CSV-filen som en liste. Du kan også lese dataene i en CSV-fil som et dictionary ved å bruke "csv.DictReader()":
```Python
with open('data.csv', 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        print(row['column1'], row['column2'])
```
Dette vil skrive ut verdiene i spesifikke kolonner for hver rad i CSV-filen.

For å skrive data til en CSV-fil, kan du bruke "csv.writer()" og skrive hver rad som en liste:
```Python
data = [
    ['John', 'Smith', '25'],
    ['Lisa', 'Brown', '32'],
    ['Mark', 'Johnson', '28']
]
with open('data.csv', 'w') as f:
    writer = csv.writer(f)
    for row in data:
        writer.writerow(row)
```
Dette vil lage en CSV-fil med tre kolonner og tre rader.

## Dypdykk
Hvis du ønsker mer kontroll over hvordan dataene dine blir skrevet til og lest fra en CSV-fil, kan du bruke "csv.Dialect" for å definere egne formateringsregler. Du kan også spesifisere forskjellige tegn som skal brukes for å separere eller omgi verdiene i filen. Dette kan være nyttig hvis du jobber med CSV-filer fra forskjellige kilder som bruker forskjellige formater.

Det er også mulig å lese og skrive CSV-filer som har spesialtegn eller internasjonale tegnsett. Du må da spesifisere et "encoding" parameter i "open()" funksjonen som tilsvarer tegnsettet i filen din.

## Se også
- [Python "csv" biblioteket dokumentasjon](https://docs.python.org/3/library/csv.html)
- [Python "io" biblioteket dokumentasjon](https://docs.python.org/3/library/io.html)
- [Tutorial: Working with CSV files in Python](https://realpython.com/python-csv/)
- [DataCamp: Working with CSV files in Python](https://www.datacamp.com/community/tutorials/python-read-csv)