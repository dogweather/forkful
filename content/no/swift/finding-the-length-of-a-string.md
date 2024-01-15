---
title:                "Finn lengden av en streng"
html_title:           "Swift: Finn lengden av en streng"
simple_title:         "Finn lengden av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du vil lage et program som kan håndtere tekst og informasjon, er en viktig del å kunne finne lengden på en tekststreng. Dette kan være nyttig for å sette begrensninger på brukerinput, filtrere ut uønskede tegn, eller bare for å få informasjon om en tekst du arbeider med.

## Slik gjør du det
Å finne lengden på en tekststreng i Swift er veldig enkelt. Alt du trenger å gjøre er å bruke metoden `count` på strengen du vil ha lengden på. La oss se på et eksempel:

```Swift
let tekst = "Hei verden!"
print(tekst.count)
```

Dette vil gi følgende utput: `12`, siden det er 12 tegn i "Hei verden!".

## Dykk dypere
Når vi brukte `count`-metoden i det første eksempelet, var det faktisk en forkortelse for `String`-typens `count`-egenskap. Dette kan virke forvirrende, men egentlig er metoder og egenskaper ganske like, og de kan brukes om hverandre i mange tilfeller.

En annen ting å merke seg er at `count`-egenskapen faktisk teller antall tegn og ikke bokstaver. Dette kan være viktig å huske hvis du bruker språk med spesielle tegn, for eksempel norsk. La oss se på et eksempel hvor vi teller antall bokstaver i en tekst med spesialtegn:

```Swift
let tekst = "Ha det / hei!"
print(tekst.count) // vil gi utput: 12
print(tekst.unicodeScalars.count) // vil gi output: 11
```

Vi kan nå se at det faktisk er 11 bokstaver i teksten, men `count`-metoden teller antall tegn, og for å få riktig antall bokstaver må vi bruke `unicodeScalars`-egenskapen i tillegg.

## Se også
- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift by Sundell: Strings](https://www.swiftbysundell.com/articles/strings-in-swift/)