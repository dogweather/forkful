---
title:                "Extrahering av substränger"
html_title:           "Swift: Extrahering av substränger"
simple_title:         "Extrahering av substränger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är helt enkelt att ta ut en mindre del av en befintlig sträng. Detta kan vara användbart för att till exempel hämta en specifik del av en text eller för att hantera en variabel i en mer specifik form. Programameras gör detta för att effektivisera och förbättra kodens läsbarhet.

## Hur man gör:
```Swift
// Enkelt exempel på hur man extraherar en substräng från en sträng
let namn = "Maria Andersson"
let förnamn = namn.prefix(5) // resultat: "Maria"
let efternamn = namn.suffix(9) // resultat: "Andersson"
```

```Swift
// Mer avancerat exempel med en if-sats
let lösenord = "hemligt123"
var förstaBokstav: String? = nil

if lösenord.count > 8 {
    förstaBokstav = String(lösenord.prefix(1)) // resultat: "h"
}
```

## Djupdykning:
Att extrahera delar av en sträng har varit en vanlig åtgärd i programmering under lång tid. Förr användes ofta substr() metoden, men den har nu ersatts med till exempel prefix() och suffix() för att öka tydligheten och läsbarheten av koden. Implementeringen av dessa funktioner varierar mellan olika programmeringsspråk och kan ha inverkan på prestandan.

## Se även:
- Läs mer om stränghantering i Swift: https://developer.apple.com/documentation/swift/string
- För en djupare förståelse av substrängar: https://en.wikipedia.org/wiki/Substring