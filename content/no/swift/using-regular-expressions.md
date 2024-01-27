---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) lar deg søke og manipulere tekst basert på mønstre. De er kraftige redskaper for tekstbehandling og data-validering som sparer tid og krefter.

## How to:
Swift gjør bruk av `NSRegularExpression` for å håndtere regex. Her ser du hvordan du søker etter ord som starter på "s":

```Swift
import Foundation

let string = "Swift skaper smarte, smidige løsninger."
let pattern = "\\bs\\w+"

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let results = regex.matches(in: string, range: NSRange(string.startIndex..., in: string))
    let words = results.map { String(string[Range($0.range, in: string)!]) }
    print(words) // Output: ["skaper", "smarte", "smidige"]
} catch {
    print("Regex feil: \(error.localizedDescription)")
}
```

## Deep Dive
Regex stammer fra 1950-tallets automaton-teori. Alternativer til regex inkluderer string-searching algoritmer som 'contains' og 'hasPrefix' i Swift. For optimalisering, bruk `lazy` for store datasett og unngå for komplekse mønstre da det kan føre til treg prosessering.

## See Also
- Swift dokumentasjon om NSRegularExpression: [Swift NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Online regex tester og verktøy: [Regex101](https://regex101.com)
- Swift standardbiblioteket for enklere søk: [Swift String](https://developer.apple.com/documentation/swift/string)
