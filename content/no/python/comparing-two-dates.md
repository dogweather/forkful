---
title:                "Python: Sammenligner to datoer"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave i programmering, spesielt når man jobber med tidsstyring eller analyse av data over tid. Ved å sammenligne to datoer kan du enkelt finne ut om de er like, større enn eller mindre enn hverandre. Dette kan være nyttig for å filtrere data eller beregne tidsdifferanser. I denne bloggposten vil jeg vise deg hvordan du enkelt kan sammenligne to datoer i Python.

## Hvordan

Vi kan sammenligne to datoer ved å bruke Python-modulen `datetime`. Først må vi importere denne modulen i koden vår:

```Python
import datetime
```

Deretter kan vi definere våre to datoer som variabler og bruke `datetime`-objektet til å konvertere dem til datoobjekter. La oss ta et eksempel der vi sammenligner to datoer for å se om den første datoen er større enn den andre:

```Python
d1 = "01/02/2020" # Første dato
d2 = "05/02/2020" # Andre dato

# Konverterer datoene til datetime-objekter
d1 = datetime.datetime.strptime(d1, "%d/%m/%Y") 
d2 = datetime.datetime.strptime(d2, "%d/%m/%Y")

# Sammenligner de to datoene
if d1 > d2:
    print("Første dato er større enn andre dato")
else:
    print("Første dato er ikke større enn andre dato")
```

Dette vil føre til følgende output:

```
Første dato er ikke større enn andre dato
```

Vi kan også sammenligne to datoer for å se om de er like ved å bruke `==`-operatøren:

```Python
if d1 == d2:
    print("Datoene er like")
else:
    print("Datoene er ikke like")
```

Dette vil gi følgende output:

```
Datoene er ikke like
```

## Deep Dive

Når vi sammenligner datoer, er det viktig å tenke på hvordan vi definerer dem. I eksempelet ovenfor definerte vi datoene som strenger, men det kan også være lurt å bruke `datetime`-objekter fra starten av. Dette kan gjøres ved hjelp av `datetime`-modulens `date`-funksjon, som tar inn år, måned og dag som parametere.

En annen viktig ting å huske på er at datoer kan være følsomme for hvilket format de er i. Dette kan løses ved å bruke `datetime`-modulens `strptime`-funksjon, som vi brukte i eksempelet over. Denne funksjonen lar oss konvertere en streng til et `datetime`-objekt ved å spesifisere formatet på datoen.

For å lære mer om manipulering av datoer i Python, kan du utforske `datetime`-modulen og dens forskjellige funksjoner og metoder.

## Se også

* [Offisiell dokumentasjon for datetime-modulen i Python](https://docs.python.org/3/library/datetime.html)
* [Enkel sammenligning av datoer i Python](https://www.geeksforgeeks.org/comparing-dates-python/)
* [Andre nyttige dato- og tidsmoduler i Python](https://realpython.com/python-date-time/)