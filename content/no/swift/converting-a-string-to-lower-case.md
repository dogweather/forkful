---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver er prosessen med å endre alle store bokstaver i en tekststreng til deres tilsvarende små bokstaver. Dette brukes ofte i programmering for å gi uniformitet til data, og for å sikre korrekt sammenligning, søk og sortering av strenger.

## Hvordan?
Her er hvordan du gjør det i Swift:

```Swift
let uppercaseString = "Hei Verden!"
let lowercaseString = uppercaseString.lowercased()
print(lowercaseString)
// Output: hei verden!
```

Du ser at 'Hei Verden!' nå er blitt til 'hei verden!'.

## Dyp Dykk
1. Historisk sett har behovet for å konvertere strenger til små bokstaver alltid vært til stede i programmering; ikke bare for å normalisere og standardisere data, men også for å sammenligne strenger effektivt. Hver bokstav i ASCII har forskjellige representasjoner for store og små bokstaver. Derfor, 'A' er ikke lik 'a' i en streng sammenligning.

2. Alternativt kan vi bruke `lowercased()` funksjonen på en streng for å få en ny streng med små bokstaver. Originalstrengen forblir upåvirket.

3. Var mustered av 'String' klassen, `lowercased()` returnerer en ny streng med alle alfabeter i små bokstaver. Denne metoden fungerer ikke bare for engelske bokstaver, men også for andre språkaldersbokstaver.

## Se Også
- [Apple Documentation: lowercased()](https://developer.apple.com/documentation/swift/string/2293301-lowercased) for ytterligere informasjon om `lowercased()` metoden.
- [Stack Overflow: Swift](https://stackoverflow.com/questions/tagged/swift) for praktiske diskusjoner om Swift programmering.