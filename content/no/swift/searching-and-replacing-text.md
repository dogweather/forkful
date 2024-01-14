---
title:                "Swift: Søke etter og erstatte tekst"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave i programmering, spesielt når man arbeider med store mengder kode. Dette kan spare deg for mye tid og gjøre programmeringen mer effektiv. 

## Hvordan

Det er flere måter å søke og erstatte tekst på i Swift. Et av de vanligste verktøyene er metoden `replacingOccurrences(of:with:)`. Her er et eksempel på hvordan den kan brukes:

```Swift

let text = "Hei, jeg er en tekststreng."
let newText = text.replacingOccurrences(of: "tekststreng", with: "programmerer")

print(newText) // Output: Hei, jeg er en programmerer.
```

I dette eksempelet erstatter vi "tekststreng" med "programmerer". Det er også mulig å bruke denne metoden for å fjerne tekst, ved å erstatte den med en tom streng: `replacingOccurrences(of:with:"")`.

En annen måte å søke og erstatte tekst på er ved hjelp av regulære uttrykk med klassen `NSRegularExpression`. Her er et eksempel på hvordan du kan erstatte gjentakende ord:

```Swift
let text = "Dette er er en en tekst tekst."
let regex = try NSRegularExpression(pattern: "\\b(\\w+) \\1\\b", options: [])
let range = NSRange(location: 0, length: text.utf16.count)
let newText = regex.stringByReplacingMatches(in: text, options: [], range: range, withTemplate: "$1")

print(newText) // Output: Dette er en tekst.
```

Her bruker vi et regulært uttrykk for å finne alle gjentakende ord og erstatte dem med en enkelt forekomst av ordet.

## Dypdykk

Det finnes også andre metoder og verktøy for å søke og erstatte tekst i Swift, som for eksempel bruk av `range` og `deletingCharacters(in:)`. Det er viktig å vite hvilken metode som passer best til ditt spesifikke tilfelle.

Det kan også være nyttig å lese dokumentasjonen og eksperimentere med forskjellige metoder for å bli mer komfortabel med søking og erstatting av tekst i Swift.

## Se også

- [Offisiell dokumentasjon for `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1642976-replacingoccurrences)
- [Guide til regulære uttrykk i Swift](https://www.raywenderlich.com/1137702-regular-expressions-tutorial-for-swift)
- [Mer informasjon om søking og erstatting i Swift](https://www.hackingwithswift.com/example-code/strings/how-to-use-regex-in-swift)