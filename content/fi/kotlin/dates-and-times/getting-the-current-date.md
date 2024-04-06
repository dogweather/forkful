---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:24.836746-07:00
description: "Kuinka: Kotlinilla ei ole omaa p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan API:a,\
  \ mutta se nojautuu Java Standard Libraryyn t\xE4m\xE4n toiminnallisuuden osalta.\
  \ N\xE4in voit hankkia\u2026"
lastmod: '2024-03-13T22:44:56.541472-06:00'
model: gpt-4-0125-preview
summary: "Kotlinilla ei ole omaa p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan API:a, mutta se\
  \ nojautuu Java Standard Libraryyn t\xE4m\xE4n toiminnallisuuden osalta."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:


### Käyttäen vakio Kotlinia
Kotlinilla ei ole omaa päivämäärän ja ajan API:a, mutta se nojautuu Java Standard Libraryyn tämän toiminnallisuuden osalta. Näin voit hankkia nykyisen päivämäärän:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Päivän päivämäärä: $today")
}
```

**Näyte tuloste:**
```
Päivän päivämäärä: 2023-04-05
```

### Käyttäen java.util.Date
Operaatioihin, jotka vaativat sekä päivämäärän että ajan, voit mieluummin käyttää `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Nykyinen päivämäärä ja aika: $currentDate")
}
```

**Näyte tuloste:**
```
Nykyinen päivämäärä ja aika: Wed Apr 05 15:20:45 GMT 2023
```

### Käyttäen Joda-Time kirjastoa
Ennen Java 8:n uuden Päivämäärä ja Aika API:n esittelyä, Joda-Time oli de-facto standardi päivämäärä-aika operaatioihin Java:ssa ja Kotlinissa. Vaikka monet projektit eivät enää tarvitse sitä, jotkin saattavat edelleen käyttää sitä perinteisistä syistä tai henkilökohtaisen mieltymyksen vuoksi.

Lisää Joda-Time kirjasto projektisi build.gradle tiedostoon:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Päivän päivämäärä: $today")
}
```

**Näyte tuloste:**
```
Päivän päivämäärä: 2023-04-05
```

### Käyttäen ThreeTenABP:tä Androidille
Android-kehityksessä suositellaan käyttämään Java Time API:n takaisinsovitusta ThreeTen Android Backport Projectin kautta kaikille versioille ennen Android API-tason 26.

Lisää riippuvuus sovelluksesi build.gradle tiedostoon:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Alusta se Application-luokassasi:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Sen jälkeen voit käyttää sitä näin:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Päivän päivämäärä: $today")
}
```

**Näyte tuloste:**
```
Päivän päivämäärä: 2023-04-05
```
