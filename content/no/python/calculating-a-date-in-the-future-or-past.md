---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Python: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor 
Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge arrangementer, følge med på viktige tidsfrister eller enkelt og greit være nysgjerrig på hvilken dag en spesiell begivenhet falt på i fortiden.

## Hvordan
For å kunne beregne en dato i fremtiden eller fortiden, må vi først importere datamodulen i Python ved hjelp av følgende kodeblokk:
```Python
import datetime
```
Deretter kan vi bruke funksjonen `datetime.date()` for å opprette en datoobjekt og på den måten regne ut fremtidige eller fortidige datoer.

For å beregne en dato 10 dager fra nå, bruker vi følgende kodeblokk:
```Python
today = datetime.date.today()
# Dagens dato
future_date = today + datetime.timedelta(days=10)
# Beregner dato 10 dager fra nå
print(future_date) 
# Printer ut resultatet
```
Vi bruker `timedelta` for å angi antall dager vi ønsker å legge til. Dette vil gi oss datoen 10 dager fra nå.

For å beregne en dato 3 måneder tilbake i tid, bruker vi tilsvarende kodeblokk:
```Python
today = datetime.date.today()
# Dagens dato
past_date = today - datetime.timedelta(days=3*30)
# Beregner dato 3 måneder tilbake i tid
print(past_date)
# Printer ut resultatet
```
I dette tilfellet bruker vi `timedelta` med antall dager multiplisert med antall måneder (3*30) for å få litt mer nøyaktig dato.

## Dypdykk
Dette er bare noen få eksempler på hvordan man kan beregne datoer i fremtiden eller fortiden. Det finnes også muligheter for å bruke `datetime` modulen til å beregne tid og tidssoner, samt utføre mer komplekse beregninger.

En viktig ting å huske på er at datoen vil bli beregnet basert på datoen og tiden på maskinen din. Dette kan føre til avvik i beregningene hvis du bruker et annet tidssone eller hvis datoen og tiden på maskinen din ikke er korrekt.

## Se også
- [Python dokumentasjon om datamodulen](https://docs.python.org/3/library/datetime.html)
- [Enkel guide til å arbeide med datoer i Python](https://opensource.com/article/18/8/easy-introduction-dates-python) 
- [Video tutorial om å arbeide med datoer i Python](https://www.youtube.com/watch?v=eirjjyP2qcQ)