---
title:    "Swift: Søke og erstatte tekst"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en grunnleggende oppgave innen programmering, og det er viktig å mestre denne ferdigheten for å optimalisere koden din og gjøre endringer raskt og effektivt.

## Hvordan å søke og erstatte tekst i Swift

For å søke og erstatte tekst i Swift, kan du bruke `replacingOccurrences(of, with:)` metoden. Denne metoden tar to parametere: en streng du ønsker å søke etter, og en streng du ønsker å erstatte med.

```
let originalTekst = "Hei, dette er en test"
let nyTekst = originalTekst.replacingOccurrences(of: "test", with: "eksempel")

print(nyTekst) // Output: Hei, dette er en eksempel
```

I dette eksempelet erstatter vi "test" med "eksempel" i den opprinnelige teksten. Det er viktig å merke seg at denne metoden returnerer en ny streng og ikke endrer den opprinnelige strengen.

Du kan også bruke `replacingOccurrences(of, with:, options:)` metoden hvis du ønsker å gjøre en case-insensitive søk og erstatning:

```
let originalTekst = "Hei, dette er en test"
let nyTekst = originalTekst.replacingOccurrences(of: "TEST", with: "eksempel", options: .caseInsensitive)

print(nyTekst) // Output: Hei, dette er en eksempel
```

## Dypere dykk

Når du bruker `replacingOccurrences(of, with:)` metoden, erstatter den alle forekomster av den angitte teksten. Dette betyr at hvis den samme teksten forekommer flere ganger i strengen, vil den bli erstattet hver gang.

Du kan også bruke `replacingOccurrences(of, with:, options:, range:)` metoden hvis du ønsker å begrense antall erstatninger som gjøres. I dette tilfellet må du angi et område av strengen som skal søkes i.

```
let originalTekst = "Hei, dette er en test"
let nyTekst = originalTekst.replacingOccurrences(of: "e", with: "a", options: [], range: originalTekst.startIndex..<originalTekst.index(originalTekst.startIndex, offsetBy: 12))

print(nyTekst) // Output: Hia, dette ar an test
```

I dette eksempelet begrenser vi søket til bare de første 12 tegnene i strengen, slik at bare de første to forekomstene av "e" blir erstattet med "a".

## Se også

- [String Manipulation in Swift](https://medium.com/swinginc/string-manipulation-in-swift-eee5248ec22a)
- [Working with Strings in Swift](https://www.hackingwithswift.com/articles/155/working-with-strings-in-swift)