---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Kapitalisering av en tekststreng betyr å gjøre bokstavene i strengen til store bokstaver. Programmerere kapitaliserer strenger for å standardisere data, for eksempel for å gjøre brukerinput konsekvent eller for å fremheve seksjoner av tekst.

## How to:
I Swift bruker du `uppercased()` for å kapitalisere en streng. Her er et raskt eksempel:

```Swift
let smallText = "norsk elghund"
let capitalizedText = smallText.uppercased()
print(capitalizedText) // Output: "NORSK ELGHUND"
```

Veldig greit, ikke sant?

## Deep Dive
Kapitalisering av strenger er viktig for å skille mellom vanlig tekst og viktig tekst, som overskrifter eller akronymer. Før Swift og moderne programmeringsspråk, trengte utviklere å manipulere ASCII-verdier direkte for å kapitalisere bokstaver. Men nå, med Swifts `uppercased()`-metode, er dette mye enklere.

Alternativer til `uppercased()` inkluderer `lowercased()` for å gjøre alt til små bokstaver og `capitalized` som gjør bare første bokstav i hvert ord til en stor bokstav. Her er et eksempel:

```Swift
let mixedText = "Norsk ELGHUND vinner løp"
print(mixedText.lowercased()) // Output: "norsk elghund vinner løp"
print(mixedText.capitalized)  // Output: "Norsk Elghund Vinner Løp"
```

Implementeringen av `uppercased()` tar hensyn til lokale innstillinger – eller 'locales' – som bestemmer riktige kapitaliseringsregler basert på språk og region.

## See Also
For videre lesing, sjekk ut Swift-dokumentasjonen:
- [String](https://developer.apple.com/documentation/swift/string/)
- [StringTransform](https://developer.apple.com/documentation/foundation/stringtransform/)
- [Locale](https://developer.apple.com/documentation/foundation/locale)

Du kan også lære mer om Unicode og kapitalisering på:
