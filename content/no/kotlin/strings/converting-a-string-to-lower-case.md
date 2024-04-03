---
date: 2024-01-20 17:38:55.159656-07:00
description: "How to: Kotlin har en integrert funksjon `toLowerCase()` for \xE5 gj\xF8\
  re om tekst til sm\xE5 bokstaver."
lastmod: '2024-03-13T22:44:40.737297-06:00'
model: gpt-4-1106-preview
summary: "Kotlin har en integrert funksjon `toLowerCase()` for \xE5 gj\xF8re om tekst\
  \ til sm\xE5 bokstaver."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

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
