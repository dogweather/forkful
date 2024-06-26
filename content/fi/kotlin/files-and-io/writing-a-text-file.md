---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:25.425553-07:00
description: "Kuinka: Kotlin tarjoaa suoraviivaisen tavan tiedostoihin kirjoittamiseen,\
  \ hy\xF6dynt\xE4en standardikirjastoa ilman, ett\xE4 tarvitaan lis\xE4kolmannen\
  \ osapuolen\u2026"
lastmod: '2024-03-13T22:44:56.549658-06:00'
model: gpt-4-0125-preview
summary: "Kotlin tarjoaa suoraviivaisen tavan tiedostoihin kirjoittamiseen, hy\xF6\
  dynt\xE4en standardikirjastoa ilman, ett\xE4 tarvitaan lis\xE4kolmannen osapuolen\
  \ kirjastoja."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Kotlin tarjoaa suoraviivaisen tavan tiedostoihin kirjoittamiseen, hyödyntäen standardikirjastoa ilman, että tarvitaan lisäkolmannen osapuolen kirjastoja. Tässä on yksinkertainen esimerkki:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin-tiedoston kirjoittaminen!"
    File("example.txt").writeText(textToWrite)
}
```
Tämä koodinpätkä luo tiedoston nimeltä "example.txt" projektin juurihakemistoon ja kirjoittaa siihen merkkijonon `Hello, Kotlin-tiedoston kirjoittaminen!`. Jos tiedosto on jo olemassa, se kirjoitetaan yli.

Haluatessasi lisätä hallitusti tekstiä tiedostoon tai kirjoittaa suurempia määriä dataa, voit käyttää `appendText` tai `bufferedWriter()`:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Lisätään lisää tekstiä."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Suuria määriä tekstiä...\nUseilla riveillä."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // Lisää tekstiä olemassa olevaan tiedostoon
    writeWithBufferedWriter() // Kirjoittaa suuren määrän tekstidataa tehokkaasti
}
```

`appendToFile`-funktiossa lisäämme lisää tekstiä "example.txt"-tiedostoon kirjoittamatta sen nykyistä sisältöä yli. `writeWithBufferedWriter`-funktio esittelee tehokkaan tavan kirjoittaa suuria määriä tekstiä tai dataa, erityisen hyödyllistä I/O-operaatioiden minimoimiseksi käsiteltäessä useita rivejä tai suuria tiedostoja.

Nämä esimerkit kattavat perustoiminnot tekstintiedostojen kirjoittamiseksi Kotlinilla, esitellen Kotlinin standardikirjaston yksinkertaisuuden ja tehokkuuden tiedosto-I/O-operaatioissa.
