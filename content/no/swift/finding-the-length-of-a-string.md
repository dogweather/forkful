---
title:                "Finne lengden på en streng"
html_title:           "Go: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Lengden på en Streng i Swift

## Hva og hvorfor?

Å finne lengden på en streng i programmering refererer til å telle antallet karakterer i den gitte strengen. Dette er nyttig, for eksempel til form validering, dataanalyse, eller manipulering av tekstdata på mange forskjellige måter.

## Slik gjør du:

Swift gjør det enkelt å finne strenglengden. Her er hvordan det kan gjøres:

```Swift
let tekst = "Hei, Norge!"
print(tekst.count)
```

Kjører du koden over vil output være `11`, som tilsvarer antallet karakterer i strengen "Hei, Norge!".

## Dypdykk

Historisk sett har måten å finne lengden på en streng variert mellom forskjellige programmeringsspråk. For eksempel i gamle versjoner av JavaScript, måtte du bruke `.length` i stedet for `.count`.

En annen viktig ting å merke seg er at Swift teller antall tegn, ikke antall unicode skalarer som noen andre språk gjør. Hvis du prøver `print("🇳🇴".count)`, vil du se resultatet `1`, ikke `2` som i noen andre språk.

Noen alternativer til `.count` i Swift inkluderer `.distance(from:to:)` og `.utf16.count`. Disse kan være nyttige i noen spesielle / avanserte tilfeller, men for det meste er `.count` den enkleste og mest direkte måten å finne antall tegn i en streng.

## Se også

For mer informasjon om strengbehandling i Swift, se:

- Swift programmeringsspråk bok: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift Standard Library: [String API Reference](https://developer.apple.com/documentation/swift/string) 

Og for mer generell informasjon om String i datavitenskap:


Husk, å vite hvordan du behandler tekstdata er en viktig del av enhver programmerers verktøykasse. Lykke til med kodingen!