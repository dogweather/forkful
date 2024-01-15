---
title:                "Søke og erstatte tekst"
html_title:           "Swift: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst kan være nyttig når du arbeider med store mengder kode eller ønsker å gjøre gjentagende endringer på tekst. Det kan også spare deg for mye tid og gjøre koden din mer effektiv.

## Hvordan

Å søke og erstatte tekst i Swift er enkelt ved hjelp av et innebygd funksjon kalt `replacingOccurrences(of:with:)`. Denne funksjonen tar to parameterverdier: teksten du ønsker å søke etter, og teksten du ønsker å erstatte den med. Her er et eksempel på hvordan du kan bruke den:

```Swift
var tekst = "Hei alle sammen!"
tekst = tekst.replacingOccurrences(of: "alle", with: "verden")
print(tekst)
```
**Output:** Hei verden!

Du kan også angi et område hvor søket skal utføres ved å bruke en `range` parameter:

```Swift
var tekst = "Det er fredag og det er sol ute."
let utvalg = tekst.range(of: "fredag")!
tekst = tekst.replacingOccurrences(of: "sol", with: "regn", options: .caseInsensitive, range: utvalg)
print(tekst)
```
**Output:** Det er regn og det er sol ute.

Som du kan se, har vi også brukt `options` parameteret for å ignorere store og små bokstaver i teksten.

## Dypdykk

I Swift kan du også utføre søk og erstatte tekst ved hjelp av regular expressions. Dette åpner for mer avanserte søkemuligheter, for eksempel å finne og erstatte alle tall i en tekststreng.

For å bruke regular expressions i Swift, kan du bruke `range(of:options:)` funksjonen og angi `.regularExpression` i options parameteret. Her er et eksempel:

```Swift
let tekst = "I dag er det 27 grader ute."
if let utvalg = tekst.range(of: "\\d+", options: .regularExpression) {
    let tall = Int(tekst[utvalg])
    print("Temperaturen er \(tall!) grader.")
}
```
**Output:** Temperaturen er 27 grader.

Regular expressions kan virke komplisert i begynnelsen, men det finnes mange ressurser og verktøy som kan hjelpe deg med å lage og teste dem.

## Se også

- [Offisiell Swift dokumentasjon](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID466)
- [Artikkel om regular expressions i Swift](https://www.bignerdranch.com/blog/regular-expressions-in-swift/)