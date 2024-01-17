---
title:                "Å bruke regulære uttrykk"
html_title:           "Swift: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions, eller regex, er et kraftig verktøy som brukes av programmører for å søke, finne og manipulere tekststrenger basert på et mønster eller en regel. Dette gjør det mulig å effektivt håndtere store mengder data, og brukes ofte i tekstbehandling, søkefunksjoner, og validering av brukerinput.

## Hvordan:
Bruk av regulære uttrykk i Swift er enkelt og kan gjøres ved å bruke den innebygde "NSRegularExpression" klasse. Her er et eksempel på hvordan du kan søke etter en e-postadresse i en tekststreng:

```Swift
let tekst = "Min e-postadresse er eksempel@eksempel.com"
let epostRegex = try! NSRegularExpression(pattern: "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}", options: .caseInsensitive)

if let match = epostRegex.firstMatch(in: tekst, options: [], range: NSRange(location: 0, length: tekst.utf16.count)) {
    let epost = (tekst as NSString).substring(with: match.range)
    print(epost)    // output: eksempel@eksempel.com
}
```

## Dykk dypere:
Regulære uttrykk har eksistert siden 1950-tallet og har blitt en standardfunksjon i de fleste programmeringsspråk. Ofte blir de brukt som en mer effektiv og nøyaktig måte å filtrere og manipulere tekst på, sammenlignet med vanlige strenger og mønstermatching. Alternativene til regulære uttrykk er vanligvis manuelle løkker og bruk av strengmanipulasjonsfunksjoner. 

I Swift brukes regulære uttrykk ved hjelp av POSIX-standarden, noe som kan være forskjellig fra andre programmeringsspråk. Men du kan forvente å finne lignende strukturer og konstruksjoner i de fleste språk som støtter regex. 

## Se også:
- [NSRegularExpression Class](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Swift Standard Library](https://developer.apple.com/documentation/swift/1695203-nsregularexpression)