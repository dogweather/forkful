---
title:                "Swift: Søking og bytting av tekst"
simple_title:         "Søking og bytting av tekst"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Uten å bruke riktig søk og erstatt-funksjonalitet i programmering kan det være en utfordring å finne og endre tekst. Dette kan føre til mye unødvendig manuelt arbeid og potensielle feil. Derfor er det viktig for utviklere å forstå hvordan man bruker søk og erstatt-funksjoner i Swift for å effektivt jobbe med store tekstfiler.

## Hvordan

For å søke og erstatte tekst i Swift, kan man bruke metoden `replacingOccurrences(of:with:)` sammen med en streng og en annen streng som skal erstattes. For eksempel:

```Swift 
let originalText = "Dette er en tekst som jeg skal endre"
let endretTekst = originalText.replacingOccurrences(of: "skal endre", with: "har endret")

print(endretTekst)
```

Dette vil resultere i følgende output: `Dette er en tekst som jeg har endret`.

Man kan også bruke regulære uttrykk gjennom klassen `NSRegularExpression` for å søke og erstatte tekst basert på et mønster. For eksempel:

```Swift
let text = "Jeg har spist 3 epler og 2 bananer"
let regex = try! NSRegularExpression(pattern: "\\d+", options: .caseInsensitive)
let modifisertTekst = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count), withTemplate: "5")

print(modifisertTekst)
```

Dette vil resultere i følgende output: `Jeg har spist 5 epler og 5 bananer`.

## Dypdykk

Det finnes flere nyttige metoder og egenskaper som kan brukes sammen med søk og erstatt-funksjonalitet i Swift. For eksempel kan man bruke flagget `caseInsensitive` for å utføre en ikke-tilpasningsdyktig søk og erstatt-operasjon, eller man kan bruke metoden `firstMatch(in: options: range:)` for å kun få det første treffet for en gitt streng.

Det kan også være nyttig å bruke kontrolstrukturen `guard` for å sikre at et søk og erstatt-operasjonen var vellykket før man gjør videre handlinger med resultatet.

## Se Også

- [Apple Developer Documentation for `String`-klassen](https://developer.apple.com/documentation/swift/string)
- [Apple Developer Documentation for `NSRegularExpression`-klassen](https://developer.apple.com/documentation/foundation/nsregularexpression)