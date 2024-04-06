---
date: 2024-01-20 17:38:52.837415-07:00
description: "How to: \"Sukellus syv\xE4lle\" Historiallisesti, merkkijonot ovat voineet\
  \ koostua erilaisista kirjainkokoista ja alustat eiv\xE4t aina ole tulkinneet niit\xE4\
  \u2026"
lastmod: '2024-04-05T21:53:58.085314-06:00'
model: gpt-4-1106-preview
summary: "\"Sukellus syv\xE4lle\" Historiallisesti, merkkijonot ovat voineet koostua\
  \ erilaisista kirjainkokoista ja alustat eiv\xE4t aina ole tulkinneet niit\xE4 samalla\
  \ tavalla."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

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
