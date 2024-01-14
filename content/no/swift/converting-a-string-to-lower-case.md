---
title:                "Swift: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

Konvertering av en streng til små bokstaver (lower case) er en viktig del av programmering da det kan bidra til å forbedre lesbarheten og funksjonaliteten til koden din. Ved å bruke lower case i strenger, blir de mer standardiserte og enklere å sammenligne med andre strenger i koden din.

# Slik gjør du det

For å konvertere en streng til lower case i Swift, kan du bruke funksjonen `lowercased()` på strengen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Swift
let navn = "ROBERT"
print(navn.lowercased())
```

Output:
```
robert
```

Her ser vi at strengen "ROBERT" er konvertert til "robert" med små bokstaver. Dette gjør det enklere å sammenligne denne strengen med en annen streng som også er konvertert til lower case.

# Dyp dykk

Når du bruker funksjonen `lowercased()` på en streng, vil alle bokstavene i strengen bli konvertert til lower case, inkludert bokstaver med aksenter og spesielle tegn. Dette gjør det enklere å håndtere ulike språk og tegn i koden din.

En annen viktig ting å merke seg er at denne funksjonen kun konverterer bokstaver i det engelske alfabetet. Andre språk, som for eksempel kinesisk eller arabisk, vil ikke bli konvertert til lower case. Det er viktig å ta hensyn til dette når du jobber med internasjonale språk i koden din.

# Se også

- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [How to Convert Strings to Lowercase in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase-in-swift)

Takk for at du leste denne bloggartikkelen om å konvertere strenger til lower case i Swift. Vi håper det har vært nyttig for deg i din programmeringsreise!