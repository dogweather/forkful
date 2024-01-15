---
title:                "Å finne lengden av en streng."
html_title:           "Python: Å finne lengden av en streng."
simple_title:         "Å finne lengden av en streng."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å finne lengden på en streng er en vanlig oppgave i programmering, og det er nyttig for å kunne håndtere og manipulere tekst data. Det er også en viktig basisfunksjon for å kunne jobbe med lister, filer og mange andre elementer i programmering.

## Slik gjør du det

```Python
# Først definerer vi en variabel med en streng
tekst = "Hei verden!"

# Deretter bruker vi Python's len() funksjon for å finne lengden av strengen
lengde = len(tekst)

# Vi kan deretter printe ut lengden, som vil gi oss tallet 12 siden det er 12 tegn i "Hei verden!"
print(lengde)

# Output:
# 12
```

Det er så enkelt! Ved å bruke len() funksjonen kan vi raskt og enkelt finne lengden av enhver streng i Python. Det er også verdt å merke seg at den også fungerer med andre datatyper som lister og tupler.

## Dypdykk

Det er interessant å vite hvordan len() funksjonen egentlig fungerer. I Python er alt en objekt, og når len() funksjonen blir kalt på en streng, så kaller den faktisk på strengens __len__() metode. Hver datatype i Python har en innebygd __len__() metode som returnerer lengden av objektet.

En annen ting å merke seg er at len() funksjonen ikke bare fungerer med strenger, men også med andre objekter som har en innebygd __len__() metode. Dette gjør det til en allsidig funksjon i programmering.

## Se også 

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Python Built-in Functions - len()](https://docs.python.org/3/library/functions.html#len)
- [The Official Python Tutorial - Strings](https://docs.python.org/3/tutorial/introduction.html#strings)