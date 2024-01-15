---
title:                "Uttrekk av delstrenger"
html_title:           "Swift: Uttrekk av delstrenger"
simple_title:         "Uttrekk av delstrenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har sannsynligvis støtt på et scenario hvor du trenger å hente ut en del av en tekststreng. Dette er hvor substring-ekstraksjon kommer inn i bildet. Det lar deg enkelt få tilgang til og manipulere en del av en tekststreng, noe som kan være nyttig i mange programmeringsscenarier.

## Slik gjør du det

For å ekstrahere en substring i Swift, kan du bruke den innebygde funksjonen `prefix`, `suffix` eller `range`. La oss se på et eksempel hvor vi skal hente ut delen av en tekststreng som begynner på indeks 3 og går til indeks 6:

```Swift
let tekst = "Hei, dette er en tekststreng"
let startIndeks = tekst.index(tekst.startIndex, offsetBy: 3)
let sluttIndeks = tekst.index(tekst.startIndex, offsetBy: 6)
let subTekst = tekst[startIndeks...sluttIndeks]

print(subTekst) // Resultat: "dett"
```

I dette eksempelet bruker vi `index`-metoden til å finne start- og sluttpunktene for vår substring. Deretter bruker vi disse indeksene til å hente ut selve substringen fra den opprinnelige tekststrengen. Hvis vi ønsker å ha et mer fleksibelt område, kan vi også bruke en `range` i stedet for å spesifisere eksplisitte indekser. For eksempel, hvis vi bare vil ha de første tre ordene i tekststrengen, kan vi gjøre følgende:

```Swift
let minRange = tekst.range(of: " ", options: .backwards,
                           range: tekst.startIndex...tekst.startIndex)
let subTekst = tekst[..<minRange!.lowerBound]

print(subTekst) // Resultat: "Hei, dette er"
```

Her bruker vi `range(of:options:range:)`-metoden til å finne posisjonen til den første mellomromstegnet. Deretter bruker vi dette intervallet til å hente ut substringen fra starten av tekststrengen til mellomrommet.

## Dypdykk

Hvis du ønsker å lære mer om hvordan Swift behandler substrings, kan du sjekke ut dokumentasjonen for `StringProtocol`-protokollen. Dette er protokollen som alle strenger i Swift implementerer, og den har mange nyttige metoder for å jobbe med substrings. Du kan også lese mer om manipulasjon av tekst i Swift i ["Strings and Characters" av Swift Programming Language Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html). 

## Se Også

- [Swift String API Reference](https://developer.apple.com/documentation/swift/string)
- [Swift Programming Language Guide](https://docs.swift.org/swift-book/)
- [Playground eksempel på substring ekstraksjon](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/GuidedTour.html)