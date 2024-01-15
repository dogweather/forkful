---
title:                "Henting av nåværende dato"
html_title:           "Python: Henting av nåværende dato"
simple_title:         "Henting av nåværende dato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Det er alltid nyttig å kunne få tak i den nåværende datoen når man programmerer. Enten det er for å registrere når en bruker opprettet en konto, for å lage dynamiske timersystemer eller bare for å få en bedre forståelse av hvor lang tid som har gått siden en hendelse. Ved å bruke Python kan man enkelt få tak i den nåværende datoen og bruke den til å utvikle mer effektive programmer.

## Slik gjør du det

For å få tak i den nåværende datoen i Python, kan man bruke innebygde funksjoner og moduler. Det er flere måter å gjøre dette på, og her vil vi vise to eksempler:

```Python
import datetime

# Få tak i den nåværende datoen ved å bruke datetime biblioteket
current_date = datetime.date.today()
print(current_date)

# Output: 2021-05-10

# Få tak i datoen og klokkeslettet ved å bruke datetime biblioteket
current_datetime = datetime.datetime.now()
print(current_datetime)

# Output: 2021-05-10 12:45:09.277694
```

## Dykk dypere

I eksemplene over brukte vi datetime biblioteket for å få tak i den nåværende datoen og klokkeslettet. Dette biblioteket har flere nyttige funksjoner som kan hjelpe deg med å manipulere datoen og klokkeslettet på ulike måter. For eksempel kan du bruke strftime() metoden for å formatere datoen og klokkeslettet på en spesifikk måte.

```Python
import datetime

# Få tak i datoen i et bestemt format
current_date = datetime.date.today().strftime("%A, %B %d, %Y")
print(current_date)

# Output: Monday, May 10, 2021
```

Det er også verdt å merke seg at datoen og klokkeslettet som blir returnert av disse funksjonene, er basert på datamaskinens nåværende dato og tid. Dette betyr at hvis datamaskinen din er satt til å bruke et annet tidssone, vil dette reflekteres i datoen og klokkeslettet som blir returnert. Man kan også bruke forskjellige funksjoner for å manipulere datoen og klokkeslettet, for eksempel å legge til eller trekke fra et gitt antall dager.

# Se også

- [Python datetime dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [Python datetime modul tutorial](https://realpython.com/python-datetime/)
- [W3Schools tutorial om Python datetimer](https://www.w3schools.com/python/python_datetime.asp)