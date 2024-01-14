---
title:    "Python: Sammenligner to datoer"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en nyttig måte å analysere og håndtere tid på i programmering. Dette kan være nyttig for å sjekke om en hendelse har skjedd før eller etter en annen, eller for å filtrere data basert på datoer.

## Hvordan

For å sammenligne to datoer i Python kan du bruke "datetime" modulen. Først må du importere modulen ved å skrive ```Python
import datetime
``` 
Deretter kan du definere to forskjellige datoer ved hjelp av "datetime" objekter, for eksempel:
```Python
date1 = datetime.datetime(2021, 5, 15)
date2 = datetime.datetime(2021, 6, 1)
```
For å sammenligne disse to datoene kan du bruke operatorer som ">", "<", ">=", "<=", "==" eller "!=", avhengig av hva du ønsker å sjekke. For eksempel:
```Python
print(date1 < date2)  # Vil returnere True siden date1 kommer før date2 i tid
```

Det er også mulig å sammenligne datoer med forskjellige format ved først å konvertere dem til "datetime" objekter ved hjelp av "strptime" funksjonen. For eksempel:
```Python
date3 = datetime.datetime.strptime("15/06/2021", "%d/%m/%Y")  # Datoen er skrevet i dd/mm/yyyy format
print(date1 < date3)  # Vil returnere False siden date1 kommer etter date3 i tid
```

## Dypdykk

For å utforske mer avanserte operasjoner med datoer, kan du sjekke ut "timedelta" objekter som gjør det mulig å legge til eller trekke fra en viss mengde tid fra en dato. Du kan også bruke "toordinal" funksjonen for å konvertere en dato til et heltall eller "strftime" funksjonen for å formatere datoer på forskjellige måter.

## Se også

- [Dato- og tidshåndtering i Python (på norsk)](https://www.learnpython.org/no/Dato_-_og_tidsh%C3%A5ndtering)
- [Python datetime dokumentasjon (på engelsk)](https://docs.python.org/3/library/datetime.html)
- [10 nyttige Python-funksjoner for å jobbe med datoer (på engelsk)](https://towardsdatascience.com/10-python-functions-to-work-with-datetime-f13b5d1971f2)