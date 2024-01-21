---
title:                "Uuden projektin aloittaminen"
date:                  2024-01-20T18:03:55.666775-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Uuden projektin aloittaminen on kuin puhtaan paperin ottaminen – mahdollisuus luoda jotakin uutta. Koodarit tekevät sen ratkaistakseen ongelmia, kokeillakseen ideoita tai oppiakseen uusia asioita.

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