---
title:    "Swift: Søking og erstattelse av tekst"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med programmering, vil du ofte støte på situasjoner hvor du må gjøre endringer i store mengder tekst. Dette kan være en tidkrevende og kjedelig oppgave hvis du gjør det manuelt. Derfor kan det være nyttig å lære om søking og bytting, som er en effektiv måte å gjøre slike endringer på.

## Hvordan

For å søke og bytte tekst i Swift, kan du bruke den innebygde funksjonen `replacingOccurrences(of:with:)`. La oss si at du har en streng som inneholder et ord du vil bytte ut med et annet. Da kan du bruke følgende kode:

```Swift
let originalStreng = "Hei, mitt navn er Jan"
let nyStreng = originalStreng.replacingOccurrences(of: "Jan", with: "Per")
print(nyStreng) // "Hei, mitt navn er Per"
```

Du kan også spesifisere at søket skal være case-sensitivt ved å legge til parameteren `options: .caseInsensitive`. Hvis du vil bytte ut alle forekomster av et ord, kan du bruke `.replaceAll` i stedet for `.replace`.

## Dykk dypere

Det finnes flere funksjoner og metoder i Swift som gir deg mer kontroll over søking og bytting av tekst. Du kan for eksempel bruke `range(of:)` for å finne ut hvor i strengen et visst ord eller tegn befinner seg. Dette kan være nyttig hvis du vil bytte ut noe i en bestemt del av strengen. Du kan også bruke `replaceSubrange(with:)` for å erstatte en del av strengen med noe annet.

## Se også

- [Swift String API](https://developer.apple.com/documentation/swift/string)
- [Søking og bytting i Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [String og NSString i Swift](https://www.vadimbulavin.com/swift-4-strings-and-nsstring/)