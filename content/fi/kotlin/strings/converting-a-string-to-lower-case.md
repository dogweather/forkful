---
date: 2024-01-20 17:38:52.837415-07:00
description: 'How to: "Kuinka:".'
lastmod: '2024-03-13T22:44:56.518116-06:00'
model: gpt-4-1106-preview
summary: '"Kuinka:".'
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
