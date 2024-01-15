---
title:                "Sammenligning av to datoer"
html_title:           "Python: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan du kan sammenligne to datoer i Python? Vel, det er flere situasjoner der dette kan være nyttig, for eksempel når du jobber med dataanalyse, datavisualisering eller annen typisk programmeringsoppgaver.

## Slik gjør du det

For å sammenligne to datoer i Python, kan du bruke "datetime" -modulen. Her er et eksempel på hvordan du kan gjøre det:

```Python
from datetime import date

dato1 = date(2020, 2, 1)
dato2 = date(2020, 2, 15)

if dato1 > dato2:
    print("Dato 1 er senere enn dato 2")
elif dato1 < dato2:
    print("Dato 1 er tidligere enn dato 2")
else:
    print("Dato 1 og dato 2 er like")
```

Dette vil gi følgende output:

```
Dato 1 er tidligere enn dato 2
```

## Dypdykk

For å forstå hvordan sammenligning av datoer fungerer i Python, er det nyttig å vite litt om datatypen "date". En dato består av år, måned og dag, og i Python-representasjonen er det mulig å bruke både positive og negative tall. For eksempel kan du bruke "-1" som et år for å representere "1 f.Kr.". Dette gjør det enkelt å sammenligne datoer fra forskjellige århundrer.

Det er også verdt å merke seg at samme dato kan ha flere ulike representasjoner. For eksempel vil både "2020-02-01" og "20-02-01" bli tolket som den 1. februar 2020. Når du gjør en sammenligning, vil Python ta hensyn til dette og gi korrekt resultat.

## Se også

- [datetime-modulen dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [Python-dokuksjonen for comparators](https://docs.python.org/3.8/library/stdtypes.html#comparisons)
- [Hvordan håndtere datoer og tid i Python](https://realpython.com/python-datetime/)