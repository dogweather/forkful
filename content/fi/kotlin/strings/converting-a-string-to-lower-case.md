---
date: 2024-01-20 17:38:52.837415-07:00
description: "\"Mit\xE4 & Miksi?\" Stringin muuttaminen pieniksi kirjaimiksi tarkoittaa\
  \ alkuper\xE4isen merkkijonon muuntamista versioksi, jossa kaikki isot kirjaimet\
  \ ovat\u2026"
lastmod: '2024-02-25T18:49:53.435400-07:00'
model: gpt-4-1106-preview
summary: "\"Mit\xE4 & Miksi?\" Stringin muuttaminen pieniksi kirjaimiksi tarkoittaa\
  \ alkuper\xE4isen merkkijonon muuntamista versioksi, jossa kaikki isot kirjaimet\
  \ ovat\u2026"
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
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
