---
date: 2024-01-20 17:54:40.917056-07:00
description: "Tekstitiedoston lukeminen tarkoittaa tiedon hakua tekstipohjaisesta\
  \ tiedostosta. Ohjelmoijat tekev\xE4t t\xE4t\xE4 esimerkiksi datan analysoinnin,\u2026"
lastmod: '2024-03-13T22:44:56.548711-06:00'
model: gpt-4-1106-preview
summary: Tekstitiedoston lukeminen tarkoittaa tiedon hakua tekstipohjaisesta tiedostosta.
title: Tekstitiedoston lukeminen
weight: 22
---

## How to:
Lue tiedosto `readText()`-funktioilla:

```Kotlin
import java.io.File

fun main() {
    val content = File("example.txt").readText()
    println(content)
}
```

Tai turvallisempi tapa `useLines()`-funktiolla, joka sulkee tiedoston automaattisesti:

```Kotlin
import java.io.File

fun main() {
    File("example.txt").useLines { lines ->
        lines.forEach { println(it) }
    }
}
```

Esimerkkitiedoston sisältö tulostuu näin:

```
Hei maailma!
Tämä on esimerkkitiedosto.
```

## Deep Dive
Tekstitiedoston lukeminen on perustoiminta ohjelmoinnissa, alkaen C-kielen `fopen`- ja `fgets`-funktioista. Kotlin tarjoaa useita helppokäyttöisiä funktioita jotka ovat turvallisia ja suorituskykyisiä. `readText()` on yksinkertainen tapa lukea pieni tiedosto, mutta ei sovi suurille tiedostoille muistinrajoituksien vuoksi. `useLines()` taas käsittelee joka rivin sitä mukaa kun lukee, säästäen muistia.

## See Also
- Kotlin API:n referenssi: [Kotlin API Reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Virallinen Kotlin-oppaita ja -tutoriaaleja: [Kotlin Tutorials](https://kotlinlang.org/docs/tutorials/)
