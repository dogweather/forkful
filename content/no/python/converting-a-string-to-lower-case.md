---
title:    "Python: Konvertering av en streng til små bokstaver"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger i programmering må man jobbe med tekst og string-variabler. Noen ganger kan det være nødvendig å konvertere disse stringene til små bokstaver, eller "lower case" på engelsk. Dette kan være nyttig for å sammenligne tekst uten å ta hensyn til store eller små bokstaver, eller for å følge vanlige konvensjoner i programmeringsspråket ditt. I denne bloggposten skal vi se på hvordan man kan gjøre dette i Python.

## Hvordan gjøre det
Konvertering av en string til lower case i Python er en enkel prosess. I vårt eksempel skal vi bruke funksjonen `.lower()`, som tar inn en string og returnerer en ny kopi av stringen med alle bokstaver omgjort til små bokstaver.

```Python
string = "Hei, denne STRINGen skal konverteres til lower case"
print(string.lower())
```

Dette vil gi oss følgende output:

```Python
hei, denne stringen skal konverteres til lower case
```

Som du ser, er alle bokstavene nå i små bokstaver. Dette kan være nyttig hvis du for eksempel ønsker å sammenligne denne stringen med en annen string som allerede er i lower case, slik at du er sikker på at de er helt like.

## Deep Dive
Det finnes også andre måter å konvertere en string til lower case i Python på. Noen programmerere kan for eksempel velge å bruke `.casefold()`-funksjonen i stedet for `.lower()`, da denne håndterer spesielle karakterer og emojis bedre. Andre vil kanskje foretrekke å bruke `.swapcase()`-funksjonen, som bytter om på små og store bokstaver. Det viktigste er å finne en metode som fungerer best for deg og ditt prosjekt.

## Se også
- [Dokumentasjon for .lower() i Python](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Dokumentasjon for .casefold() i Python](https://docs.python.org/3/library/stdtypes.html#str.casefold)
- [Dokumentasjon for .swapcase() i Python](https://docs.python.org/3/library/stdtypes.html#str.swapcase)