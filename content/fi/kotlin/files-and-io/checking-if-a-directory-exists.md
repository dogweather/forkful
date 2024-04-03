---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:47.755635-07:00
description: "Miten: Kotlin, joka toimii JVM:n p\xE4\xE4ll\xE4, hy\xF6dynt\xE4\xE4\
  \ Java File API:a tiedosto-operaatioihin, tehden hakemiston olemassaolon tarkistuksista\
  \ suoraviivaisia.\u2026"
lastmod: '2024-03-13T22:44:56.545820-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, joka toimii JVM:n p\xE4\xE4ll\xE4, hy\xF6dynt\xE4\xE4 Java File\
  \ API:a tiedosto-operaatioihin, tehden hakemiston olemassaolon tarkistuksista suoraviivaisia."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Miten:
Kotlin, joka toimii JVM:n päällä, hyödyntää Java File API:a tiedosto-operaatioihin, tehden hakemiston olemassaolon tarkistuksista suoraviivaisia. Tässä on perusesimerkki:

```kotlin
import java.io.File

fun main() {
    val path = "/path/to/directory"
    val directory = File(path)

    if (directory.exists() && directory.isDirectory) {
        println("Hakemisto on olemassa: $path")
    } else {
        println("Hakemistoa ei ole olemassa: $path")
    }
}
```
Esimerkkitulostus, olettaen että hakemisto on olemassa:
```
Hakemisto on olemassa: /path/to/directory
```
Ja jos sitä ei ole:
```
Hakemistoa ei ole olemassa: /path/to/directory
```

Kotlin-projektissa saatat myös usein työskennellä Kotlin-spesifisten kirjastojen tai viitekehysten kanssa, kuten Ktor web-sovelluksille tai kotlinx.coroutines asynkroniseen ohjelmointiin. Kuitenkin, hakemiston olemassaolon tarkistamiseen, standardi Java `File` API kuten esitetty on tyypillisesti riittävä ja laajalti käytetty Kotlinin yhteentoimivuuden vuoksi Javan kanssa. Tähän spesifiin tehtävään ei tarvita kolmannen osapuolen kirjastoja, mikä tekee siitä saavutettavan ja suoraviivaisen aloittelijoille, jotka siirtyvät muista ohjelmointikielistä Kotliniin.
