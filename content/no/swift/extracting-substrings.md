---
title:                "Utvinne delstrenger"
html_title:           "Swift: Utvinne delstrenger"
simple_title:         "Utvinne delstrenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hva er egentlig uttrekking av substringer? Kort sagt, det er å få tak i deler av en tekststreng i et program. Det kan være nyttig når du for eksempel ønsker å manipulere en del av en tekst eller hente ut spesifikk informasjon fra en større tekststreng. Programmerere bruker dette ofte for å gjøre koden sin mer effektiv og for å automatisere handlinger som ellers ville tatt mye tid og krefter å gjøre manuelt.

## Hvordan:
For å uttrekke en substring i Swift, bruker vi funksjonen ```substring(from: )```. Her er et eksempel som viser hvordan vi kan bruke denne funksjonen for å få tak i delen av teksten "verden" fra strengen "Hei verden!":

```Swift
let tekst = "Hei verden!"
let substring = tekst.substring(from: 4)
print(substring)
```

Output:
```verden!```

Som du ser, så teller vi starten av strengen som index 0, dermed må vi bruke tallet 4 for å få tak i "verden". Du kan også uttrekke en substring ved å angi både start- og sluttpunktet, som vist i dette eksempelet:

```Swift
let tekst = "Blomster er fine"
let substring = tekst.substring(from: 0, to: 7)
print(substring)
```

Output:
```Blomster```

## Dykk dypere:
Uttrekking av substringer har vært en del av programmering i lang tid, og er en vanlig funksjon i mange programmeringsspråk. I tillegg til å bruke ```substring(from: )``` i Swift, kan du også bruke ```prefix(_:)``` og ```suffix(_:)``` for å uttrekke deler av en streng basert på begynnelsen eller slutten av teksten. Det finnes også forskjellige algoritmer og metoder som kan brukes for å uttrekke substringer, avhengig av hva slags oppgave du ønsker å løse.

## Se også:
Hvis du ønsker å lære mer om uttrekking av substringer i Swift, kan du sjekke ut disse ressursene:

* [Apple-dokumentasjon: Substrings](https://developer.apple.com/documentation/swift/string/2894564-substring)
* [Swift by Example: Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID265)
* [Ray Wenderlich: Substrings in Swift](https://www.raywenderlich.com/221-swift-3-strings-and-characters)