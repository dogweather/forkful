---
title:    "Python: Å bruke regulære uttrykk"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Bruk av regulære uttrykk kan effektivisere søk og manipulasjon av tekstbaserte data. Ved å lære å bruke regulære uttrykk kan du raskt finne og endre bestemte deler av tekst eller data i ditt Python-program.

## Hvordan

For å bruke regulære uttrykk i Python, må du importere `re` biblioteket. Deretter kan du bruke forskjellige funksjoner fra dette biblioteket for å søke og manipulere tekst. Her er et eksempel på en tekststreng og hvordan vi kan bruke regulære uttrykk for å finne alle forekomster av ordet "oransje".

```python 
import re

tekst = "Jeg elsker å spise oransje frukt. Appelsiner er min favoritt!"

resultater = re.findall("oransje", tekst)
print(resultater)
```

Output:
```
["oransje", "oransje"]
```

Vi ser at vi får en liste med to forekomster av ordet "oransje" i teksten vår. Dette er fordi vi brukte `re.findall()`-funksjonen som returnerer alle forekomster av et mønster i en tekststreng. Du kan også bruke regulære uttrykk for å erstatte deler av en tekststreng med et annet ord eller uttrykk. Her er et eksempel på hvordan du kan erstatte alle forekomster av ordet "kul" med "fantastisk" i en tekststreng:

```python
tekst = "Den nye telefonen min er veldig kul!"

resultater = re.sub("kul", "fantastisk", tekst)
print(resultater)
```

Output:
```
"Den nye telefonen min er veldig fantastisk!"
```

Det finnes mange forskjellige funksjoner og muligheter for å bruke regulære uttrykk i Python, så det er lurt å lese mer om dette og øve på å bruke det i ditt eget kodearbeid.

## Dypdykk

Regulære uttrykk kan virke forvirrende og komplekse i starten, men det er en kraftig verktøykasse for tekstbehandling i Python. Det er bygd på et sett av regler og spesialtegn som representerer mønstre i en tekststreng. For eksempel representerer `.` ethvert tegn, `*` representerer null eller flere forekomster av det som kommer før, og `+` representerer en eller flere forekomster av det som kommer før.

For å lære mer om hvordan du kan bruke regulære uttrykk i Python, kan du sjekke ut dokumentasjonen og eksemplene på nettet. Prøv også å eksperimentere med forskjellige mønstre og se hvordan de påvirker søket eller manipulasjonen av teksten.

## Se også

- [Python dokumentasjon for regulære uttrykk](https://docs.python.org/3/library/re.html)
- [Tutorial om regulære uttrykk i Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)