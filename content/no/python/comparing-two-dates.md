---
title:                "Python: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor sammenligne to datoer: En enkel guide for å forstå datoenes viktighet

Når man jobber med data eller utvikler programmer, er det ofte nødvendig å sammenligne to datoer for å ta beslutninger eller utføre beregninger. Dette kan virke som en enkel oppgave, men det er faktisk en viktig del av programmering. I denne bloggposten vil vi forklare hvorfor sammenligning av to datoer er så viktig, og guide deg gjennom hvordan det kan gjøres i Python.

## Hvordan gjøre det i Python:
Å sammenligne to datoer kan gjøres ved å bruke Python sin innebygde funksjon "date.today ()" for å hente dagens dato. Deretter kan du bruke en annen innebygd funksjon "datetime.timedelta" for å manipulere datoer og utføre sammenligninger.

```Python
from datetime import date, timedelta 
today = date.today() # hente dagens dato
tomorrow = today + timedelta(days=1) # legg til en dag
if tomorrow > today: # sammenligning av to datoer
    print("I morgen er en ny dag!")
```
Output:
```
I morgen er en ny dag!
```

Hvis du har to spesifikke datoer som en streng, kan du bruke "datetime.strptime" for å konvertere det til et "datetime" objekt og deretter sammenligne dem. Du kan også formatere datoene på forskjellige måter ved å bruke "strftime" funksjonen.

```Python
import datetime
date1 = datetime.datetime.strptime('2021-07-18', '%Y-%m-%d') # konverterer strengen til et datetime objekt
date2 = datetime.datetime.strptime('2021-08-25', '%Y-%m-%d')
if date2 > date1: # sammenligning av to datoer
    print(date2.strftime("%d.%m.%Y")) # formattere datoen til ønsket format
```
Output:
```
25.08.2021
```

## Dykk dypere:
Når man sammenligner to datoer, er det viktig å være oppmerksom på at datoer kan ha forskjellige formater og tidsforskyvninger. Det er derfor lurt å konvertere datoene til et standardformat før man utfører sammenligninger. I tillegg kan det også være nyttig å bruke funksjoner som "timedelta" for å håndtere forskjellige tidsintervaller som dager, timer eller minutter.

En annen ting å være oppmerksom på er at datoenes nøyaktighet kan variere. For eksempel kan man sammenligne to datoer på år, måned eller dag nivå, men ikke alltid på time eller sekund nivå. Det er derfor viktig å være klar over hva som er den minste felles enheten for de to datoene du ønsker å sammenligne.

## Se også:
- [Python datetime biblioteket](https://docs.python.org/3/library/datetime.html)
- [Vanlige datoformater](https://www.w3schools.com/python/python_datetime.asp)
- [Stack Overflow spørsmål om sammenligning av datoer i Python](https://stackoverflow.com/questions/325933/determine-whether-an-input-string-is-a-valid-date)

Takk for at du leste denne bloggposten om sammenligning av to datoer i Python. Vi håper det gjør det enklere for deg å håndtere datoer i dine programmer. Lykke til med kodingen!