---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få nåværende dato i programmering handler om å hente den nåværende datoen fra systemet hvor programmet kjører. Dette er nyttig i mange sammenhenger, som loggføring, tidsstempling av data, og å vise dato til brukere.

## Slik gjør du det:
Du kan få nåværende dato i Python ved å bruke `datetime`-modulen. Her er en enkel linje med kode som viser hvordan du kan gjøre dette:

```Python
from datetime import date

# Få dagens dato
dagens_dato = date.today()

print("Dagens dato:", dagens_dato)
```
Når du kjører dette programmet vil det skrive ut dagens dato.

## Dypdykk
Før Python 2.3 ble datetime-modulen introdusert, brukte Python-biblioteket `time`-modulen for å håndtere dato og tid. `datetime`-modulen forenkler mange operasjoner og er nå standard for de fleste Python-programmer.

I tillegg til `datetime`-modulen, finnes det også alternative moduler som `arrow`, `pendulum`, og `delorean` som gir flere funksjoner og en mer intuitiv API.

Når du bruker `today()`-metoden fra `date`-klassen, utføres en systemkall til datamaskinens klokke for å hente dagens dato. Denne datoen er deretter representert som et `date`-objekt som kan manipuleres videre.

## Se Også
1. Python `datetime` modul dokumentasjon: https://docs.python.org/3/library/datetime.html
2. `arrow` modul: https://arrow.readthedocs.io/en/latest/
3. `pendulum` modul: https://pendulum.eustace.io/docs/
4. `delorean` modul: https://delorean.readthedocs.io/en/latest/