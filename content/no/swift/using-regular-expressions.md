---
title:                "Swift: Å bruke regulære uttrykk"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor?
Hvis du noen gang har prøvd å finne et bestemt mønster eller ord i en større tekst, har du kanskje blitt frustrert av å måtte søke manuelt. Her kommer regular expressions (regex) til unnsetning! Ved hjelp av regex kan du søke etter, finne og manipulere tekst på en mer effektiv måte.

## Hvordan
For å bruke regular expressions i Swift, må du først importere `Foundation` biblioteket. Deretter kan du definere et regex-mønster ved hjelp av `NSRegularExpression` og sette det til en `NSRegularExpression`-variabel.

```Swift
import Foundation

let regex = try NSRegularExpression(pattern: "Swift", options: [])
```

Etter å ha definert mønsteret, kan du bruke `firstMatch(in:options:range:)`-metoden for å finne første forekomst av mønsteret i teksten.

```Swift
let text = "Swift er et fantastisk programmeringsspråk."

if let match = regex.firstMatch(in: text, options: [], range: NSRange(text.startIndex..., in: text)) {
    print(text[Range(match.range, in: text)!])
}

// Output: Swift
```

Du kan også bruke regex for å finne og erstatte tekst ved hjelp av `replaceMatches(in:options:range:withTemplate:)`-metoden.

```Swift
let newText = regex.stringByReplacingMatches(in: text, options: [], range: NSRange(text.startIndex..., in: text), withTemplate: "Kotlin")

print(newText)

// Output: Kotlin er et fantastisk programmeringsspråk.
```

## Dypdykk
Regex kan virke komplisert og forvirrende i begynnelsen, men ved å bli komfortabel med mønstre og metoder, kan du effektivisere og forenkle tekstbehandling i dine programmer. For mer avanserte mønstre og muligheter, sjekk ut [dokumentasjonen til NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression) og [regex nettsteder](https://regex101.com/).

## Se også
- [Offisiell Swift dokumentasjon](https://swift.org/documentation/)
- [En introduksjon til regular expressions på norsk](https://medium.com/@knutigro/en-introduksjon-til-regular-expressions-p%C3%A5-norsk-9ac738c94411)
- [Regex tutorial med eksempler i Swift](https://www.raywenderlich.com/5768-an-introduction-to-regular-expressions)