---
title:                "Kotlin: Merkkijonon ensimmäinen suuri kirjain"
simple_title:         "Merkkijonon ensimmäinen suuri kirjain"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi kapitalisoida string

Kapitalisointi tarkoittaa merkkijonon muuttamista niin, että jokaisen sanan ensimmäinen kirjain on isolla kirjaimella ja muut kirjaimet pienillä. Tämä voi olla hyödyllistä esimerkiksi käyttäjänimiä tai otsikoita luodessa. 

## Miten tehdä

Kotlinissa stringien kapitalisointi on helppoa. Voit käyttää `capitalize()`-metodia, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja muut kirjaimet pieniksi. Esimerkiksi:

```Kotlin
val name = "sanni"
println(name.capitalize())
```

Tulostus: "Sanni"

Jos sinulla on useita sanoja sisältävä merkkijono, voit käyttää `split()`-metodia ja `map()`-funktiota muuttaaksesi jokaisen sanan ensimmäisen kirjaimen isoksi. Esimerkiksi:

```Kotlin
val sentence = "tervetuloa kotisivuilleni"
val capitalized = sentence.split(" ").map { it.capitalize() }.joinToString(" ")
println(capitalized)
```

Tulostus: "Tervetuloa Kotisivuilleni"

## Syvempi sukellus

Kapitalisointiin liittyy muutamia seikkoja, jotka on hyvä huomioida. Esimerkiksi `capitalize()`-metodi muuttaa vain ensimmäisen kirjaimen isoksi ja jättää muut kirjaimet ennalleen. Jos haluat, että kaikki kirjaimet ovat samassa muodossa, voit käyttää `toLowerCase()` tai `toUpperCase()`-metodeja ennen kapitalisointia.

Lisäksi kannattaa pitää mielessä, että kapitalisointi on kieliriippuvainen. Esimerkiksi suomen kielessä `capitalize()`-metodi toimii oletuksena oikein, mutta toisissa kielissä se voi aiheuttaa ongelmia. Kannattaa siis aina tarkistaa, että kapitalisointi toimii halutulla tavalla.

## Katso myös

- [Kotlindoc: String.capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlindoc: String.split()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/split.html)
- [Kotlindoc: String.map()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/map.html)
- [Kotlindoc: String.joinToString()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/join-to-string.html)