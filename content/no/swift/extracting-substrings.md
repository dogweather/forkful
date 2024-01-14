---
title:    "Swift: Uthenting av delstrenger"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang sittet med en lang streng av tekst og ønsket å bare få tak i en del av den? For eksempel, du har en tekststreng som inneholder et telefonnummer og du vil bare få tak i selve nummeret uten noe ekstra informasjon. Her kommer substring-ekstrahering til nytte!

## Hvordan

Det er flere måter å ekstrahere substrings i Swift på, avhengig av hva du trenger å hente ut. La oss ta en titt på noen eksempler:

```Swift
// Opprett en tekststreng
let string = "Velkommen til Swift! Dette er en tekststreng."
```
```Swift
// Ekstraher kun "Swift" fra strengen
let substring1 = string.dropFirst(12).prefix(5)
// substring1 = "Swift"
```

Her bruker vi både `dropFirst()` og `prefix()` metoder for å hente ut den delen av strengen vi trenger. Vi bruker `dropFirst()` for å fjerne de første 12 tegnene, og deretter bruker vi `prefix()` for å få tak i de neste 5 tegnene.

```Swift
// Ekstraher nummeret fra en streng som inneholder et telefonnummer
let phoneNumber = "+47 12345678"
let startIndex = phoneNumber.firstIndex(of: "+")!
let endIndex = phoneNumber.firstIndex(of: " ")!
let phoneNumberSubstring = phoneNumber[startIndex...endIndex]
// phoneNumberSubstring = "+47"
```

Her bruker vi `firstIndex()` for å finne indeksene til "+" og mellomrommet i strengen, og deretter bruker vi dette til å ekstrahere den delen vi vil ha ved å bruke sluttindeksen som en grense.

## Dypdykk

Det er mange metoder som kan brukes for å ekstrahere substrings i Swift, blant annet `dropFirst()`, `prefix()`, `suffix()`, `firstIndex()` og `substring()`. Husk at det også er mulig å kombinere disse metodene for å få akkurat den delen av strengen du ønsker.

Det er også viktig å merke seg at substrings i Swift er forskjellig fra vanlige strenger, da substrings ikke oppretter en ny instans, men heller refererer til en del av en eksisterende streng. Dette kan være viktig å huske på ved manipulasjon av tekststrenger.

## Se også

- [Swift.org - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hva er en substring i Swift?](https://stackoverflow.com/questions/35044559/what-is-a-substring-method-in-swift)