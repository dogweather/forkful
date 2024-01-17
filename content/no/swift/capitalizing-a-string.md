---
title:                "Store bokstaver i et streng"
html_title:           "Swift: Store bokstaver i et streng"
simple_title:         "Store bokstaver i et streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hva betyr det egentlig å "kapitalisere en streng" når vi snakker om programmering? Det kan vi tenke på som å gjøre den første bokstaven i en tekst stor. Programmere gjør dette for å gjøre tekst mer lesbar og enklere å skille mellom ord og setninger.

## Hvordan:
Vi kan enkelt kapitalisere en streng ved å bruke `.capitalized` metoden på en tekstvariabel. For eksempel:
```Swift
let tekst = "velkommen til swift tips!"
print(tekst.capitalized)
```
Dette vil gi outputen "Velkommen Til Swift Tips!".

## Dyp Dykk:
Historisk sett har kapitalisering av tekst vært en viktig del av skriving og lesing. I programmering, spesielt når vi arbeider med brukergrensesnitt, er det viktig å ha en konsistent formatering av tekst for å gjøre den mer lesbar for brukeren. Andre alternativer for å kapitalisere en streng inkluderer `.uppercased` og `.lowercased` metoder, som gjør hele teksten stor eller liten uavhengig av den opprinnelige formateringen. For implementasjonsdetaljer, kan vi se nærmere på hvilke regler som brukes for å bestemme hva som skal kapitaliseres og hva som ikke skal.

## Se også:
For mer informasjon om kapitalisering av strenger, kan du sjekke ut følgende ressurser:
- [Apple official documentation - String Manipulation] (https://developer.apple.com/documentation/swift/string_manipulation)
- [Swift By Sundell - Capitalizing Strings] (https://www.swiftbysundell.com/tips/capitalizing-strings/)