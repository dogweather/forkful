---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Swift: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr rett og slett å endre alle bokstavene i en streng til små bokstaver i stedet for store bokstaver. Dette gjøres ofte av programmere for å gjøre tekstbehandling enklere og for å sikre konsistens i tekstformatering.

## Hvordan:
Å konvertere en streng til små bokstaver i Swift er enkelt. Du kan bruke "lowercased ()" -metoden på strengen din for å gjøre dette. Se eksemplene nedenfor for å se dette i aksjon.

```Swift
//Eksempel 1:
let navn = "Marie"
let lavereNavn = navn.lowercased()

print (lavereNavn) //Output: marie

//Eksempel 2:
let setning = "Jeg liker å programmere"
let lavereSetning = setning.lowercased()

print (lavereSetning) //Output: jeg liker å programmere
```

## Dypdykk:
Å konvertere en streng til små bokstaver er en vanlig oppgave for programmere. Det er spesielt nyttig når du jobber med sammenligninger av tekst, da det sikrer at sammenligningen er basert på bare de faktiske bokstavene og ikke på versaler og små bokstaver. Det finnes andre måter å konvertere strenger til små bokstaver i Swift, som for eksempel å bruke "uppercased ()" -metoden for å konvertere til store bokstaver, eller "capitalized ()" -metoden for å konvertere til store bokstaver ved første bokstav i setningen.

## Se også:
For mer informasjon om string-manipulasjon i Swift, sjekk ut Swift-dokumentasjonen: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID0E8O4STFY