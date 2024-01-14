---
title:                "Python: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å finne lengden på en streng (string) er en viktig og grunnleggende del av programmering med Python. Ved å kunne finne lengden på en streng vil du kunne behandle og manipulere tekst på en mer effektiv måte i programmene dine. Det er også en av de mest brukte funksjonene i Python, så det er viktig å ha en god forståelse av hvordan det fungerer.

## Slik gjør du det
```Python
# Definer en streng
tekst = "Hei, dette er en streng!"

# Bruk len() funksjonen for å finne lengden på strengen
lengde = len(tekst)

# Skriv ut resultatet
print("Lengden på strengen er:", lengde)
```

Output:
```
Lengden på strengen er: 25
```

## Dykk dypere

Funksjonen `len()` i Python returnerer antall tegn i en streng. Dette inkluderer mellomrom, spesialtegn og symboler. Det er viktig å være klar over dette når du bruker funksjonen, da det kan ha konsekvenser i noen tilfeller. For eksempel, hvis du bruker `len()` for å sjekke om en streng er tom, vil du få en falsk verdi hvis strengen bare inneholder mellomrom.

En nyttig funksjon å bruke sammen med `len()` er `str.strip()`. Denne funksjonen fjerner alle mellomrom og ekstra tegn fra starten og slutten av en streng, noe som kan være nyttig når du skal telle antall "ekte" tegn.

## Se også

- [Offisiell Python dokumentasjon om len() funksjonen](https://docs.python.org/nl/3/library/stdtypes.html#str.len)
- [En guide til å lære Python fra scratch på norsk](https://realpython.com/python-beginner-tips-no/)