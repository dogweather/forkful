---
title:    "Python: Sammenligner to datoer"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor 

En av de vanligste oppgavene i programmering er å sammenligne to datoer. Dette er nyttig for å utføre ulike funksjoner som å sortere, filtrere eller lage rapporter basert på datoer. Det kan også hjelpe deg med å forstå forskjellen mellom to datoer og beregne tidsintervaller. Uansett hva grunnen er, er det viktig å kunne sammenligne to datoer effektivt i Python.

## Hvordan

Det er flere måter å sammenligne to datoer på i Python, avhengig av hvilket format du jobber med. Her er noen eksempler som viser hvordan du kan gjøre dette for forskjellige datotyper:

### Datoobjekter

```Python
#from datetime import er allerede inkludert i Python for å jobbe med datoer

first_date = datetime.date(2021, 3, 12)
second_date = datetime.date(2021, 3, 15)

#Sammenligner to datoobjekter ved å bruke "==" operatøren
if first_date == second_date:
    print("Datoene er like.")
else:
    print("Datoene er ikke like.")

#Output: Datoene er ikke like.
```

### Strenger

Datoer kan også representeres som strenger, i forskjellige formater som "dag/måned/år" eller "måned/dag/år". Her er et eksempel på hvordan du kan sammenligne to datoer som strenger:

```Python
first_date = "3/12/2021"
second_date = "03/15/2021"

#Sammenligner to strenger ved å bruke "==" operatøren
if first_date == second_date:
    print("Datoene er like.")
else:
    print("Datoene er ikke like.")

#Output: Datoene er ikke like.
```

## Deep Dive

Når du sammenligner datoer i Python, er det viktig å huske på at mindre detaljer som klokkeslett ikke blir tatt med i sammenligningen, kun datoene. Dette betyr at når du sammenligner datoer, vil Python bare se på datoen og ikke klokkeslettet. Det kan også være utfordrende å sammenligne datoer på forskjellige formater, spesielt hvis de er i strengformat. Det er derfor anbefalt å konvertere datoene til datoobjekter før du sammenligner dem for å unngå feil.

## Se også

- [The datetime module in Python documentation](https://docs.python.org/3/library/datetime.html)
- [How to compare two dates in Python on Stack Overflow](https://stackoverflow.com/questions/3277380/how-to-compare-two-dates-in-python)
- [Python Dates and Time on w3schools](https://www.w3schools.com/python/python_datetime.asp)