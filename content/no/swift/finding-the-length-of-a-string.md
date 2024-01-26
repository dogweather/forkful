---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:48:24.964862-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å finne lengden av en streng er å måle antallet tegn den inneholder. Programmerere gjør dette for data validering, grensesjekking og når man manipulerer tekstbasert data.

## Slik gjør du:
Swift gjør det enkelt å finne lengden av en streng. Her er et eksempel:

```Swift
let greeting = "Hei, verden!"
print(greeting.count)
```

Output vil være:

```
12
```

Dette er fordi `greeting` inneholder 12 tegn.

## Dypdykk
Å finne strengens lengde kan virke rett frem, men det er et par ting å ha i tankene. For det første, Swift bruker Unicode-skalare for sine strenger, noe som gjør det mulig å representere tegn fra mange forskjellige språk. Tidligere, med ASCII eller ANSI, var strenglengde og byte-størrelse ofte det samme, men i Unicode-verdenen er dette ikke tilfelle. Derfor teller `.count` i Swift Unicode-skalare, ikke bytes.

Alternativer til `.count` kan inkludere å bruke utf8 eller utf16 egenskapene til en streng hvis du trenger byte-størrelsen for nettverksoverføring eller lagring.

Implementasjonsdetaljer å merke seg er at `.count` er en O(n) operasjon fordi den må gå gjennom hele strengen for å telle tegn, så det kan være mindre effektivt for veldig lange strenger.

## Se også
- Swift dokumentasjon på strenger: [Swift String](https://developer.apple.com/documentation/swift/string)
- Unicode Standard: [Unicode](http://www.unicode.org/)
- Swift bok av Apple: [The Swift Programming Language](https://docs.swift.org/swift-book/)
