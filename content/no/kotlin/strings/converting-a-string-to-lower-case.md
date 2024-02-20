---
date: 2024-01-20 17:38:55.159656-07:00
description: "Omgj\xF8ring av tekst til sm\xE5 bokstaver betyr \xE5 konvertere alle\
  \ bokstavene i en streng til deres sm\xE5 bokstaver. Programmerere gj\xF8r dette\
  \ for \xE5 standardisere\u2026"
lastmod: 2024-02-19 22:04:59.984637
model: gpt-4-1106-preview
summary: "Omgj\xF8ring av tekst til sm\xE5 bokstaver betyr \xE5 konvertere alle bokstavene\
  \ i en streng til deres sm\xE5 bokstaver. Programmerere gj\xF8r dette for \xE5 standardisere\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## What & Why?
Omgjøring av tekst til små bokstaver betyr å konvertere alle bokstavene i en streng til deres små bokstaver. Programmerere gjør dette for å standardisere data, for eksempel når man sammenligner brukerinput selv om det er skrevet med stor eller liten bokstav.

## How to:
Kotlin har en integrert funksjon `toLowerCase()` for å gjøre om tekst til små bokstaver.

```Kotlin
fun main() {
    val original = "Hei, Verden!"
    val lowerCased = original.lowercase()
    println(lowerCased)
}
```

Eksempel output:
```
hei, verden!
```

## Deep Dive
Fra Kotlin 1.5, er `toLowerCase()` erstattet av `lowercase()` for å unngå forvirring med språkspesifikke tegn. Dette sikrer at konverteringen følger Unicode standarder.

Alternativer:
- `String.lowercase(Locale)`: Bruk denne hvis behov for språkspesifikk omgjøring.
- Manuell iterasjon og konvertering av hver karakter, men ikke anbefalt da det er tidkrevende og feilutsatt.

Implementasjonsdetaljer:
- `lowercase()` bruker Unicode's "case mapping" regler, som tar høyde for spesielle tilfeller som tyske eszett (ß) som omgjøres til "ss".

## See Also
Interesserte kan se:
- [Kotlin Standard Library Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Unicode Case Folding](https://www.unicode.org/reports/tr44/#CaseFolding)
