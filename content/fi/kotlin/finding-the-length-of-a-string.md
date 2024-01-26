---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:34.339380-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Stringin pituuden selvittäminen tarkoittaa merkkijonon merkkien lukumäärän laskemista. Ohjelmoijat tekevät tämän, kun heidän täytyy käsitellä tekstin pituutta tai leikata ja vertailla merkkijonoja.

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
