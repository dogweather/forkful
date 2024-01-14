---
title:                "Python: Å finne lengden til en streng"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg med å finne lengden på en streng i Python? Vel, det kan virke som en enkel oppgave, men å forstå hvordan dette fungerer kan hjelpe deg med å bli en bedre programmerer. Det er også en nyttig ferdighet å ha når du jobber med manipulering av tekstdata.

## Hvordan
For å finne lengden på en streng i Python, kan du bruke `len()` funksjonen. La oss si at vi har følgende streng:
```
tekst = "Hei, verden!"
```
For å finne lengden på denne strengen, skriver vi følgende kode:
```
print(len(tekst))
```
Dette vil gi følgende output:
```
13
```

Du kan også bruke `len()` funksjonen på andre datatyper, som for eksempel lister, tupler eller dictionaries. La oss si at vi har en liste med navn:
```
navn = ["Anna", "Per", "Sara", "Markus"]
```
Her kan vi bruke `len()` funksjonen til å finne antall elementer i listen:
```
print(len(navn))
```
Dette vil gi følgende output:
```
4
```

## Dypdykk
Nå lurer du kanskje på hvordan `len()` funksjonen faktisk fungerer. Det er en innebygd funksjon i Python som returnerer antall elementer i en gitt objekt. Det betyr at hvis du bruker `len()` på en streng, vil det returnere antall tegn i strengen. Hvis du bruker det på en liste, vil det returnere antall elementer i listen.

Det er også verdt å nevne at `len()` funksjonen ikke bare fungerer på tekstdata, men også på numeriske og boolske verdier. For eksempel kan du bruke den til å finne antall sifre i et tall, eller antall elementer i en boolsk liste.

## Se Også
- [Offisiell Python Dokumentasjon for `len()`](https://docs.python.org/3/library/functions.html#len)
- [GeeksforGeeks Tutorial: `len()` funksjonen i Python](https://www.geeksforgeeks.org/python-len-function/)
- [Stack Overflow: "Hvordan finne lengden på en streng i Python?"](https://stackoverflow.com/questions/6706830/how-to-get-the-length-of-a-string)