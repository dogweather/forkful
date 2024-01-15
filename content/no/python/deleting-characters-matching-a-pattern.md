---
title:                "Slette tegn som matcher et mønster"
html_title:           "Python: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang kjørt en kode og blitt møtt med en lang liste over uønskede tegn i resultatet? Fortvil ikke, det er en enkel måte å fjerne disse tegnene ved å bruke Python.

## Slik gjør du det
```
import re 
tekst = "Hei! @Dette er en tekst med uønskede tegn#." 
resultat = re.sub('[^A-Za-z0-9 ]+', '', tekst) 
print(resultat)
```

Dette vil fjerne alle tegn som ikke er bokstaver, tall eller mellomrom fra teksten, og resultatet blir "Hei Dette er en tekst med uønskede tegn".

## Dykk dypere
Dette eksempelet bruker det innebygde "re" biblioteket i Python for å søke etter en bestemt mønster i en streng. Mønsteret [^A-Za-z0-9 ]+ brukes for å identifisere uønskede tegn, og disse tegnene blir deretter erstattet med en tom streng ved hjelp av re.sub() funksjonen.

En annen måte å fjerne uønskede tegn på er å bruke en løkke og sjekke hvert tegn individuelt. Dette kan være nyttig hvis du kun ønsker å fjerne visse tegn, og ikke alle som ikke er bokstaver, tall eller mellomrom.

## Se også
- [Dokumentasjon for re-modulen i Python](https://docs.python.org/3/library/re.html)
- [Tutorial: Lær å bruke regulære uttrykk i Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)