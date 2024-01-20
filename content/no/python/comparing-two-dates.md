---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer er rett og slett prosessen med å bestemme hvilken dato som kommer før eller etter den andre. Dette hjelper programmerere med å håndtere tidsfrister, gjøre tidsseriefunksjoner, eller utføre tidssensitiv statistikk.

## Hvordan:

Python standardbiblioteket datetime tillater direkte sammenligning mellom datoer. 

Her er et eksempel på hvordan du sammenligner to datoer:

```Python
from datetime import date

dato1 = date(2021, 12, 1)
dato2 = date(2021, 12, 24)

if dato1 < dato2:
    print("dato1 kommer før dato2")
else:
    print("dato2 kommer før eller er lik dato1")
```

Utskrift for koden vil være:

```Python
dato1 kommer før dato2
```

## Dyp Dykk:

Historisk sett, i tidlige programmeringsspråk, var sammenligning av datoer en utfordrende oppgave. Fortsatt, i moderne språk som Python, gjør innebygde date-time funksjonene det til en enkel oppgave.

Alternativt, bibliotek som Pandas gir også funksjoner for å sammenligne datoer og håndtere tidsserier. 

Når det gjelder implementering, lagrer Python datoobjekter som antall sekunder siden 1. januar 1970, en praksis kjent som 'epoch' tid. Dette gjør at Python kan sammenligne datoer direkte ved å sammenligne sekundene siden 'epoch'.

## Se Også:

For mer informasjon, sjekk ut disse nyttige ressursene:

1. Offisielle Python dokumentasjon: [datetime modul](https://docs.python.org/3/library/datetime.html)
2. For videre lesing: [Pandas dokumentasjon på tidsseriedata](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html)
3. For diffing datoer: [dateutil biblioteket](https://dateutil.readthedocs.io/en/stable/)
4. Tutorials: [Python Date and Time Tutorial](https://realpython.com/python-datetime/) Veggen dypere forklaring og flere eksempler.