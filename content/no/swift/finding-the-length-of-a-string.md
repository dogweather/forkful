---
title:                "Swift: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å finne lengden på en streng er en vanlig oppgave i programmering, og kan være nyttig for å håndtere tekstbaserte data og utføre forskjellige operasjoner på dem.

# Slik gjør du det

For å finne lengden på en streng i Swift, kan du bruke den innebygde funksjonen `count`, som returnerer antall tegn i strengen. For eksempel:

```swift
let tekst = "Hei, verden!"
let lengde = tekst.count
print(lengde)
```

Dette vil produsere følgende utdata i konsollen:

```
13
```

Som du kan se, teller funksjonen også mellomrom og spesialtegn som del av strengens lengde.

# Dypdykk

Det er viktig å merke seg at `count`-funksjonen returnerer en `Int`-verdi, som kun kan håndtere strenger med en størrelse på opptil 2,147,483,647 tegn. Dette er imidlertid sjelden et problem i praksis, da en streng med en slik lengde ville være veldig vanskelig å håndtere.

Det finnes også andre måter å finne lengden på en streng, som for eksempel å bruke en løkke og telle antall tegn manuelt. Men dette er en mer tungvint og mindre effektiv metode.

# Se også

- [String og Characters i Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Introduksjon til Swift-programmering](https://developer.apple.com/swift/blog/?id=2)
- [Offisiell Swift-dokumentasjon](https://docs.swift.org/)