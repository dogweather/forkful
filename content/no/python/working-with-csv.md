---
title:                "Python: Å arbeide med csv"
simple_title:         "Å arbeide med csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

CSV-filer er en av de vanligste måtene å lagre og dele data på. Ved å jobbe med CSV-filer i Python, kan man enkelt lese og manipulere data for ulike formål, som for eksempel å lage grafer, lage tabeller eller analyse. Det er en viktig ferdighet å ha i et stadig mer datadrevet samfunn, enten man er en erfaren programvareutvikler eller en nybegynner.

## Hvordan

For å jobbe med CSV-filer i Python, må man først importere det innebygde "csv" biblioteket:

```Python
import csv
```

For å åpne en CSV-fil, bruker man funksjonen "open" og spesifiserer filnavnet og hvor man ønsker å lese filen. Det er også viktig å spesifisere lesetilgangen som "r" for lesing:

```Python
with open('data.csv', 'r') as f:
    reader = csv.reader(f)
```

Etter å ha åpnet og lest filen, kan man begynne å arbeide med dataene. Man kan for eksempel lese og skrive ut kolonneoverskrifter og data til konsollen:

```Python
with open('data.csv', 'r') as f:
    reader = csv.reader(f)
    # Leser og skriver ut kolonneoverskrifter
    headers = next(reader)
    print(headers)
    # Leser og skriver ut data
    for row in reader:
        print(row)
```

Output:

```
['Navn', 'Alder', 'Favorittfarge']
['Ole', '25', 'Blå']
['Kari', '30', 'Grønn']
['Per', '28', 'Rød']
```

Man kan også gjøre ulike manipulasjoner på dataene, som å filtrere, sortere eller regne ut gjennomsnittet av tallverdier. Her er et eksempel der vi sorterer dataene basert på alder og skriver ut resultatet til en ny CSV-fil:

```Python
with open('data.csv', 'r') as f:
    reader = csv.reader(f)
    # Sorterer dataene etter andre kolonne (alderskolonnen)
    sorted_data = sorted(reader, key=lambda row: int(row[1]))
    # Skriver ut dataene til en ny CSV-fil
    with open('sorted_data.csv', 'w') as new_f:
        writer = csv.writer(new_f)
        # Skriver inn kolonneoverskrifter
        writer.writerow(headers)
        # Skriver inn den sorterte dataen til filen
        for row in sorted_data:
            writer.writerow(row)
```

## Dypdykk

Når man jobber med CSV-filer i Python, er det viktig å ha kunnskap om hvordan dataene er strukturert. En CSV-fil består av rader og kolonner, og hver celle er delt av et komma. Det kan også være tilfeller der dataene er atskilt av en annen spesifisert skilleverdi, som et semikolon. Det er også viktig å være klar over at dataene kan være av ulike datatyper, som tekststrenger, tall eller datoer, og man må derfor håndtere dem på en hensiktsmessig måte.

Det er også viktig å være bevisst på at CSV-filer kan variere i størrelse og kompleksitet. Noen filer kan være små og enkle, mens andre kan være store og inneholde mange kolonner og rader. Det kan også være tilfeller der dataene er feilformaterte eller mangler, og man må derfor kunne håndtere disse situasjonene på en effektiv måte.

## Se også

- [Offisiell Python dokumentasjon for csv-modulen](https://docs.python.org/3/library/csv.html)
- [Real Python tutorial for working with CSV files in Python](https://realpython.com/python-csv/)
- [Eksempel på CSV-filer for å øve på å jobbe med det](https://github.com/datsoftlyngby/soft2020fall-bi-materials/tree/master/04-Exercise)