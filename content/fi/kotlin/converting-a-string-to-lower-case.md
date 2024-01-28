---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:38:52.837415-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"
Stringin muuttaminen pieniksi kirjaimiksi tarkoittaa alkuperäisen merkkijonon muuntamista versioksi, jossa kaikki isot kirjaimet ovat pieniä. Ohjelmoijat käyttävät tätä yhdenmukaistaessaan tekstiä, esimerkiksi vertailuja tai tietojen syötön validointia varten.

## How to:
"Kuinka:"
```kotlin
fun main() {
    val originalString = "Hei Maailma!"
    val lowercaseString = originalString.lowercase()
    println(lowercaseString)
}
```
Tulostuu:
```
hei maailma!
```

## Deep Dive
"Sukellus syvälle"

Historiallisesti, merkkijonot ovat voineet koostua erilaisista kirjainkokoista ja alustat eivät aina ole tulkinneet niitä samalla tavalla. Muunnos pieniksi kirjaimiksi varmistaa tasaisuuden.

Vaihtoehtoisesti käytettävissä on myös `lowercase(Locale)`, jos tarvitaan tiettyyn asetukseen perustuvaa muunnosta.

Merkkijonojen muuttamiseen liittyvät funktiot käyttävät `Char`-tyypin metodeja, kuten `toLowercase()`, mutta Kotlinin standardikirjaston `lowercase()`-funktio hoitaa koko merkkijonon kerrallaan.

## See Also
"Näytä lisäksi"

- Java-kirjallisuus Unicode-merkkien käsittelystä, joka voi valaista Kotlininkin taustoja: [Unicode Standard](http://unicode.org/standard/standard.html)
