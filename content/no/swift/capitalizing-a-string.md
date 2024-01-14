---
title:    "Swift: Kapitalisering av en streng"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Å kunne kapitalisere en streng er en viktig ferdighet for programvareutviklere, da det ofte er nødvendig å formatere tekst i forskjellige applikasjoner. Enten det er å lage en tittel, en undertekst eller en meny, er det viktig å kunne style tekst for å gjøre den mer lesbar og presentabel.

## Slik gjør du det

For å kapitalisere en streng i Swift, kan du bruke funksjonen `capitalized` på en `String`-variabel eller innebygd streng. Her er et eksempel på hvordan dette kan gjøres:

```Swift
let navn = "ole"
print(navn.capitalized)
// Output: Ole
```

Det er også mulig å kapitalisere bare den første bokstaven i en streng, ved hjelp av funksjonen `prefix` og `suffix` i kombinasjon med `capitalized`:

```Swift
var beskrivelse = "en enkel setning"
beskrivelse.prefix(1).capitalized + beskrivelse.suffix(beskrivelse.count-1)
// Output: En enkel setning
```

## Dypdykk

Under overflaten bruker `capitalized` funksjonen `localizedCapitalized` for å ta hensyn til språkkontekst og ulike former for å kapitalisere teksten, avhengig av språkinnstillingene på enheten.

## Se også

For mer informasjon om å arbeide med strenger i Swift, kan du sjekke ut følgende ressurser:

- [Swift Strings officielt dokument](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Hvordan bruke Strings i Swift](https://www.hackingwithswift.com/sixty/1/1/how-to-use-strings-in-swift)