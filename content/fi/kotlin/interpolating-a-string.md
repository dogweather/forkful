---
title:                "Merkkijonon interpolointi"
html_title:           "Kotlin: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Interpoloiminen on keino lisätä dynaamista sisältöä tekstin sisään ja saada näin aikaan interaktiivisia viestejä. Monet ohjelmoijat käyttävät sitä luodakseen muuttuvia muuttujia ja lausekkeita, jotka mukautuvat erilaisiin tilanteisiin.

Kuinka:

```Kotlin
val greeting = "Hei!"
val name = "John"
val message = "${greeting}, ${name}! Oletko valmis aloittamaan ohjelmoinnin?"
println(message)
```
Tuloste:
```
Hei, John! Oletko valmis aloittamaan ohjelmoinnin?
```

Syvä sukellus:

Interpoloiminen on ollut käytössä ohjelmoinnissa jo pitkään, erityisesti C-kielen "printf" -funktiota käytetään interpoloimaan merkkijonoja. Vaihtoehtoisia tapoja lisätä dynaamista sisältöä ovat esimerkiksi muuttujien lisääminen tekstiin "+" -merkillä tai funktioiden käyttäminen.

Tärkeää tietoa interpoloimisesta on, että syöte erotetaan merkkijonoista dollarinmerkillä "$". Tätä käytetään siirtääkseen muuttujan arvo merkkijonon sisälle. Voit myös käyttää "\" -merkkiä estämään merkkijonon erikoismerkit.

Katso myös:

- [String interpolointi Kotlinissa](https://kotlinlang.org/docs/reference/basic-types.html#string-interpolation)
- [Python-formaattimerkkijonot](https://realpython.com/python-string-formatting/#3-python-string-interpolation-with-format)