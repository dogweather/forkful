---
title:                "Organisering av kode i funksjoner"
aliases:
- no/swift/organizing-code-into-functions.md
date:                  2024-01-26T01:12:02.421873-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å gruppere kode i funksjoner er å bryte ned oppgaver i gjenbrukbare deler. Det gjør koden ren, mindre utsatt for feil, og enklere å feilsøke eller restrukturere.

## Hvordan gjøre det:
Forestille deg en oppgave: kalkulere gjennomsnittet av en array. Uten funksjoner, ville du klemt det hele inn i main. Med funksjoner, ville du gjort dette:

```swift
func beregnGjennomsnitt(av tall: [Double]) -> Double {
    let sum = tall.reduce(0, +)
    return tall.isEmpty ? 0 : sum / Double(tall.count)
}

// Bruk
let poengsummer = [92.5, 88.75, 99.0, 70.5]
let gjennomsnittspoengsum = beregnGjennomsnitt(av: poengsummer)
print("Gjennomsnittlig poengsum er \(gjennomsnittspoengsum)")
```

Eksempel på utskrift ville være: 
```
Gjennomsnittlig poengsum er 87.6875
```

## Dypdykk
Historisk sett, som programmering ble mer kompleks, ble funksjoner en hjørnestein for håndtering av kompleksitet. Alternativer inkluderer inline-koding og kopiering-liming av kode (spagettikode) – nå stort sett vurdert som dårlig praksis. I Swift, er funksjoner første klasses borgere; de kan tildeles til variabler, sendes som argumenter, og returneres fra andre funksjoner, noe som gjør kode mer modulær og fleksibel.

Når det kommer til implementasjon, design dine funksjoner for å gjøre en ting godt. Sikte på funksjoner med et klart formål og et navn som reflekterer dette. Pass på antall parametere—for mange og du gjør sannsynligvis for mye. Feilbehandling? Vurder å bruke funksjoner som kaster unntak og håndtere problemer nådig. Husk: Swift handler om lesbarhet og enkel vedlikehold.

## Se også
- [Swift Programming Language Guide - Funksjoner](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray Wenderlichs Swift stilkilde](https://github.com/raywenderlich/swift-style-guide)
- [Martin Fowlers Refactoring: Forbedring av Designet på Eksisterende Kode](https://martinfowler.com/books/refactoring.html)
