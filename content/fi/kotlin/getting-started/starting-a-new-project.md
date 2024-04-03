---
date: 2024-01-20 18:03:55.666775-07:00
description: "Uuden projektin aloittaminen on kuin puhtaan paperin ottaminen \u2013\
  \ mahdollisuus luoda jotakin uutta. Koodarit tekev\xE4t sen ratkaistakseen ongelmia,\u2026"
lastmod: '2024-03-13T22:44:56.531269-06:00'
model: gpt-4-1106-preview
summary: "Uuden projektin aloittaminen on kuin puhtaan paperin ottaminen \u2013 mahdollisuus\
  \ luoda jotakin uutta."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: (Kuinka tehdään:)
Aloita uusi Kotlin-projekti. Asenna ensin IntelliJ IDEA. Käynnistä se, ja luo uusi projekti:

```Kotlin
// Valitse tiedostovalikosta "New Project"
// Valitse Kotlin/JVM ja määritä projektillesi nimi ja sijainti
// Paina "Finish"
```

Luo ensimmäinen tiedosto `Main.kt`:

```Kotlin
fun main() {
    println("Hello, new Kotlin project!")
}
```

Suorita se:

```Kotlin
// Näet konsolissa:
Hello, new Kotlin project!
```

## Deep Dive (Sukellus syvyyksiin):
Kotlin ilmestyi vuonna 2011, ja JetBrains kehitti sen selkeämpänä ja turvallisempana Java-alternatiivina. Kotlin sopii Android-sovelluskehitykseen ja palvelinpuolen sovelluksiin. Projektia luodessa voit valita tyypin kuten JVM, Android, JS tai Native. Vaaka- ja pystyprojektien rakenne auttaa pitämään koodin järjestyksessä. Gradle tai Maven hoitaa riippuvuudet, ja useimmat projektit käyttävät niitä.

## See Also (Katso myös):
- Kotlinin virallinen dokumentaatio (kattaa projektialoituksesta kaikkeen): [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- Gradlen dokumentaatio Kotlin-projekteille: [Gradle Kotlin DSL](https://docs.gradle.org/current/userguide/kotlin_dsl.html)
