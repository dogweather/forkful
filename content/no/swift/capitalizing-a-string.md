---
title:                "Sette stor bokstav i en streng"
html_title:           "Swift: Sette stor bokstav i en streng"
simple_title:         "Sette stor bokstav i en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kapitalisering av en streng betyr å gjøre den første bokstaven i hver eneste ord i strengen stor. Programmers gjør dette for å forbedre lesbarheten og legge til estetisk verdi til en tekst. 

## Hvordan gjør man det:

Her er et kodeeksempel i Swift:

```Swift
let navn = "henrik ibsen"
let kapitalisertNavn = navn.capitalized
print(kapitalisertNavn)
```

En kjøringssekvens av koden ovenfor vil gi:

```
Henrik Ibsen
```

## Dypdykk:

Historisk kontekst: kapitalisering av strenger har vært et vanlig verktøy i typografi i hundrevis av år. 

Alternativer: Swift gir en alternativ metode, `uppercased()`, som gjør alle tegnene i strengen til store bokstaver. 

Implementeringsdetaljer: I Swift skjer kapitalisering ved å gå gjennom strengen ord for ord, og konvertere det første tegnet i hvert ord til en stor bokstav.

## Se også:

For mer informasjon om strengmanipulasjon i Swift, ta en titt på disse lenkene:

- [Offisiell Swift dokumentasjon](https://docs.swift.org)
- [Strengmanipulasjon Guide fra Ray Wenderlich](https://www.raywenderlich.com/449-swift-string-manipulation-operators-and-more)
- [Stack Overflow Diskusjon om Strengkapitalisering](https://stackoverflow.com/questions/26364914)