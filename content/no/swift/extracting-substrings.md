---
title:                "Swift: Utvinning av delstrenger"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

### Hvorfor

Hvorfor ville man ønske å ekstrahere substrings i Swift programmering? Ofte når vi jobber med tekstbehandling, er det nødvendig å få tak i deler av en tekststreng for å utføre ulike operasjoner på den. Ved å ekstrahere substrings kan man enkelt hente ut akkurat den informasjonen man trenger fra en lang tekststreng.

### Hvordan gjør man det

For å ekstrahere en substring i Swift, kan man bruke funksjonen `prefix(_:)` og `suffix(_:)`. Disse funksjonene tar inn en parameter som angir hvor mange tegn man ønsker å ekstrahere fra enten starten eller slutten av tekststrengen.

```Swift
let tekststreng = "Hei verden"
let prefix = tekststreng.prefix(2)
let suffix = tekststreng.suffix(3)

print(prefix) // Output: He
print(suffix) // Output: den
```

Man kan også bruke funksjonen `substring(with:)` for å spesifisere et område (range) av tekststrengen man ønsker å ekstrahere. Denne funksjonen tar imot et `Range<String.Index>` objekt som parameter, og dette kan enkelt opprettes ved å angi start og sluttindekser for området man ønsker å ekstrahere.

```Swift
let navn = "Thomas"
let subset = navn.substring(with: 1...3)

print(subset) // Output: hom
```

### Dykk dypere

En nyttig metode for å ekstrahere en substring er `dropFirst()` og `dropLast()` funksjonene. Disse funksjonene fjerner henholdsvis det første eller siste tegnet i tekststrengen og returnerer den resterende delen. Dette kan være spesielt nyttig når man skal fjerne for eksempel mellomrom i en tekststreng.

Man kan også bruke `split(separatedBy:)` funksjonen for å splitte tekststrengen inn i en array av substrings basert på et gitt tegn eller en gitt tekststreng.

### Se også

- [Offisiell Swift dokumentasjon om substring manipulation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID335)
- [Utdypende guide om bruk av substring i Swift](https://www.hackingwithswift.com/articles/114/swift-strings-and-swift-substrings-a-complete-tutorial)