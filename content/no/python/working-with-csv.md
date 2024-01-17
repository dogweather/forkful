---
title:                "Å jobbe med csv"
html_title:           "Python: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
CSV står for Comma Separated Values og er en filformat brukt til å lagre og organisere data i tabulær form. Det er spesielt nyttig for programmerere som trenger å behandle store mengder data på en strukturert måte, for eksempel å importere eller eksportere data til og fra en database.

# Slik gjør du det:
Å jobbe med CSV-filer er enkelt i Python ved hjelp av det innebygde biblioteket 'csv'. Først må du importere dette biblioteket og åpne filen du vil jobbe med. Deretter kan du lese inn dataene og behandle dem ved hjelp av de ulike funksjonene som tilbys i biblioteket. Til slutt må du ikke glem å lukke filen når du er ferdig med å jobbe med den.

```python
import csv

with open('filnavn.csv', 'r') as fil:
    data = csv.reader(fil)
    for rad in data:
        # Gjør nødvendige operasjoner med data her
        print(rad)
```

Dette eksempelet åpner en CSV-fil med lesertilgang og skriver ut hver rad i filen til skjermen. Det er ulike måter å behandle dataene på, for eksempel kan du velge å lagre dem i en liste for videre bruk.

# Dypdykk:
CSV-formatet ble utviklet på 1970-tallet og har siden blitt standard for utveksling av data mellom ulike applikasjoner. Det finnes også alternative filformater, som f.eks. TSV (tab separated values) og JSON (JavaScript Object Notation), men CSV er fortsatt mye brukt på grunn av sin enkelhet.

Når du jobber med CSV i Python, er det viktig å være oppmerksom på formateringen av dataene. Hvis cellene inneholder komma eller andre spesielle tegn, må disse håndteres riktig for å unngå feil i lesingen av filen. Det finnes også flere alternative måter å lese og skrive til CSV-filer på, avhengig av behovene til ditt spesifikke prosjekt.

# Se også:
Offisiell dokumentasjon for 'csv' biblioteket i Python: https://docs.python.org/3/library/csv.html

En oversikt over ulike filformater og hvordan de kan behandles i Python: https://realpython.com/python-csv/

Et eksempelprosjekt som viser hvordan man kan bruke CSV i et praktisk scenario: https://github.com/insafiodigital/CSV-to-JSON-converter