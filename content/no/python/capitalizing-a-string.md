---
title:                "Python: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å gjøre en bokstav i en streng stor har mange bruksområder i Python-programmering. Det kan være nyttig når du ønsker å lage en tittel, for eksempel "Hei, dette er tittelen min". Det kan også være nyttig når du jobber med tekstbehandling og trenger å formatere teksten din.

## Hvordan

Det er veldig enkelt å gjøre en bokstav i en streng stor i Python. Alt du trenger å gjøre er å bruke metoden "upper()" på strengen du ønsker å endre. La oss se et eksempel:

```python
navn = "ikke store bokstaver"
print(navn.upper())
```

Dette vil gi følgende utskrift:

```python
IKKE STORE BOKSTAVER
```

Som du kan se, gjør "upper()" metoden alle bokstavene i strengen vår store. Men hva om vi bare ønsker å gjøre den første bokstaven stor? Da kan vi bruke metoden "capitalize()":

```python
navn = "bare den første bokstaven"
print(navn.capitalize())
```

Dette vil gi følgende utskrift:

```python
Bare den første bokstaven
```

Som du kan se, gjorde "capitalize()" metoden kun den første bokstaven i strengen vår stor. Dette er nyttig når du ønsker å formatere teksten din på en bestemt måte.

## Dypdykk

Nå som du har lært hvordan du kan gjøre bokstaver store i en streng, la oss se litt dypere på hvordan dette fungerer. I Python, er bokstaver og symboler bare tall som representerer dem i ASCII-tabellen. Dermed, når du bruker "upper()" eller "capitalize()" metoden, endres disse tallene til de som representerer store bokstaver i ASCII-tabellen.

En annen ting å merke seg er at disse metodene bare fungerer på bokstaver, og ikke på tall eller andre symboler. Du vil også merke at disse metodene ikke endrer selve strengen, men returnerer en ny streng med endringene. Det betyr at du må lagre den endrede strengen i en variabel for å bruke den senere i koden din.

## Se også

- [Python strings](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [ASCII table](https://www.asciitable.com/)