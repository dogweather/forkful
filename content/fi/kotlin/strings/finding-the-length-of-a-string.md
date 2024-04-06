---
date: 2024-01-20 17:47:34.339380-07:00
description: "Kuinka: Stringin pituuden selvitt\xE4minen on ollut perusominaisuutena\
  \ ohjelmointikieliss\xE4 jo varhain, koska sen avulla voidaan hallita ja validoida\u2026"
lastmod: '2024-04-05T21:53:58.088979-06:00'
model: gpt-4-1106-preview
summary: "Stringin pituuden selvitt\xE4minen on ollut perusominaisuutena ohjelmointikieliss\xE4\
  \ jo varhain, koska sen avulla voidaan hallita ja validoida sy\xF6tteit\xE4."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## Kuinka:
```kotlin
fun main() {
    val exampleString = "Hei maailma!"
    val length = exampleString.length
    println("Merkkijonon pituus on: $length")  // Output: Merkkijonon pituus on: 12
}
```

## Syväsukellus
Stringin pituuden selvittäminen on ollut perusominaisuutena ohjelmointikielissä jo varhain, koska sen avulla voidaan hallita ja validoida syötteitä. Kotlinissa `.length`-ominaisuus palauttaa `Int`-tyypin arvon, joka kertoo merkkijonon pituuden. Vaihtoehtoisesti, vanhemmissa kielissä kuten C:ssä, merkkijonon loppu tunnistetaan nolla-terminaattorilla, ja pituuden laskeminen vaatii toistoloopin. Javassa ja Kotlinissa tämä on yksinkertaistettu, ja suorituskykyn optimoinnin lisäksi kehittäjäkokemus on parantunut.

## Katso Myös
- Kotlinin virallinen dokumentaatio merkkijonoille: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
