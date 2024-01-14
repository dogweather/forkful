---
title:                "Swift: Finne lengden av en streng"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en viktig del av Swift programmering. Dette kan være nyttig når du jobber med tekstbehandling, formatering og manipulering av data. Ved å lære hvordan man finner lengden på en streng, kan du forbedre dine ferdigheter som Swift-utvikler og gjøre koden din mer effektiv.

## Hvordan

Det finnes forskjellige måter å finne lengden på en streng i Swift på. La oss se på noen eksempler nedenfor:

```Swift
let tekst = "Hei, verden!"
print("Lengden på strengen er \(tekst.count)") 
```

Dette vil skrive ut "Lengden på strengen er 13". Som du kan se, bruker vi `count` metoden for å finne antall tegn i strengen.

Du kan også bruke `String`-konstruktøren til å finne lengden på en streng ved å konvertere den til et `String`-objekt og deretter bruke metoden `count`:

```Swift
let streng = "Dette er en streng"
let lengde = String(streng).count 
print("Lengden på strengen er \(lengde)") 
```

Dette vil også gi deg samme resultat.

## Dypdykk

Når man jobber med internasjonalisering og l10n (lokaliserings) i Swift, er det viktig å være klar over at lengden på en streng kan variere avhengig av språk. Dette skyldes at noen språk bruker flere tegn enn andre for å danne ord og setninger. Derfor er det viktig å bruke metoden `count` i stedet for `characters.count` for å få en nøyaktig lengde på en streng.

## Se også

- [Offisiell Swift dokumentasjon om lengden på strenger](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID291)
- [Enkel introduksjon til Swift-programmering på norsk](https://www.appcoda.com/swift-programmering/)
- [Lær Swift med interaktive øvelser](https://www.hackingwithswift.com/100)