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

"## Hva & Hvorfor?"

Sammenligning av to datoer er en vanlig oppgave i programmering, der man ønsker å finne ut av relasjonen mellom to tidsperioder. Dette kan være nyttig for å sortere informasjon, filtrere data eller gjøre beregninger basert på tid. 

Programmerere bruker sammenligning av datoer for å effektivt håndtere tidsrelaterte data og utføre operasjoner som avhenger av tid. Dette lar dem lage mer presise og nøyaktige programmer.

"## Hvordan:"

```Python
# Eksempel på å sammenligne to datoer
from datetime import date

date1 = date(2021, 1, 15) # Første dato
date2 = date(2021, 1, 20) # Andre dato

# Sammenligner datoer og lagrer resultatet i en variabel
resultat = date1 < date2 

print(resultat) 
```

```Python
# Output
True
```
I dette eksempelet bruker vi Python-modulen "datetime" for å lage to datoer. Deretter bruker vi operatoren "<" for å sammenligne datoene og lagrer resultatet i en variabel. Vi skriver ut resultatet for å se om den første datoen er mindre enn den andre.

"## Dykk Ned":

Sammenligning av datoer kan spores tilbake til tidlige datamaskiner, hvor det ble brukt for å håndtere tid og programmeringsspråk som brukte datoer som del av sin syntaks.

En alternativ måte å sammenligne datoer på er å bruke timestamp, som representerer antall sekunder siden 1. januar 1970. Dette gjør det enklere å sammenligne to datoer uten å måtte håndtere formatering og forskjellige tidszoner.

I Python støttes også ulike formater for datoer, som "datetime", "date" og "time", som gir forskjellig funksjonalitet for å håndtere datoer og klokkeslett.

"## Se Også":

For å lære mer om sammenligning av datoer i Python, sjekk ut disse ressursene:

- Offisiell dokumentasjon for datetime-modulen: https://docs.python.org/3/library/datetime.html
- Enkel håndbok for datoer og klokkeslett i Python: https://towardsdatascience.com/working-with-datetime-in-python-tips-and-tricks-2d8fcb815639
- YouTube-video med en live-koding av sammenligning av datoer i Python: https://www.youtube.com/watch?v=kh4MhttG7e4