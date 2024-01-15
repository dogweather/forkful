---
title:                "Slette tegn som matcher et mønster"
html_title:           "Swift: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger kan vi møte på situasjoner hvor vi trenger å slette bestemte tegn i en rekke med tekst. Dette kan være for å fjerne uønsket formatering, filtrere ut sensitive data, eller rense tekst før den skal brukes videre.

## Hvordan
For å slette tegn i en tekststreng i Swift, kan vi bruke funksjonen `replacingOccurrences(of:with:)` som lar oss erstatte alle forekomster av et bestemt tegn eller tegnmønster med en annen tekst. La oss se på et eksempel:

```Swift
let tekst = "Jeg elsker å spise epler"
let endretTekst = tekst.replacingOccurrences(of: "e", with: "")
print(endretTekst)
```

Dette vil gi oss utskriften `Jg lskr å spis aplr`, hvor alle bokstavene "e" er blitt slettet fra teksten. Vi kan også bruke denne funksjonen til å fjerne enkelttegn eller spesifikke ord, som vist i dette eksempelet:

```Swift
let setning = "Å gå ut og nyte naturen er en flott måte å lade opp på"
let endretSetning = setning.replacingOccurrences(of: "på", with: "")
print(endretSetning)
```

Dette vil gi oss utskriften `Å gå ut og nyte naturen er en flott måte å lade opp`.

## Dypdykk
For å slette tegn fra en tekststreng bruker `replacingOccurrences(of:with:)` funksjonen en algoritme kalt "finite-state automata" for å finne og erstatte tegnene som matcher det spesifiserte mønsteret. Denne algoritmen er effektiv og kan håndtere store tekster raskt. Det er også mulig å bruke regulære uttrykk for å finne og erstatte komplekse mønstre.

## Se også
- [Apple's dokumentasjon om string manipulasjon](https://developer.apple.com/documentation/foundation/string_manipulation)
- [Tutorial om bruk av regulære uttrykk i Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
- [Mer informasjon om "finite-state automata" algoritmen](https://www.educative.io/blog/finite-state-machines-explained)