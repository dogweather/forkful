---
date: 2024-01-20 17:51:06.115435-07:00
description: "How to (Kuinka tehd\xE4): String-interpolaatio on saanut inspiraation\
  \ muiden ohjelmointikielten vastaavista ominaisuuksista. Esimerkiksi Pythonin\u2026"
lastmod: '2024-04-05T22:51:10.676985-06:00'
model: gpt-4-1106-preview
summary: String-interpolaatio on saanut inspiraation muiden ohjelmointikielten vastaavista
  ominaisuuksista.
title: Merkkijonon interpolointi
weight: 8
---

## How to (Kuinka tehdä):
```Kotlin
fun main() {
    val name = "Jukka"
    val age = 35
    val greeting = "Hei, nimeni on $name ja olen $age vuotta vanha."
    
    println(greeting) // Tulostaa: Hei, nimeni on Jukka ja olen 35 vuotta vanha.
    
    // Expressions in string templates:
    val about = "Vuosi on nyt ${2023 - age}, eli olen syntynyt vuonna ${2023 - age}."
    println(about) // Tulostaa: Vuosi on nyt 1988, eli olen syntynyt vuonna 1988.
}
```

## Deep Dive (Syväsukellus):
String-interpolaatio on saanut inspiraation muiden ohjelmointikielten vastaavista ominaisuuksista. Esimerkiksi Pythonin f-merkkijonot ja JavaScriptin template literals. Kotlin esittelee `$`-merkin käyttämisen muuttujan nimiin ja `${}` rakenteen lausekkeille merkkijonojen sisällä.

Vaihtoehtoja interpolaatiolle ovat merkkijonojen kokonaan yhteenliittäminen tai `String.format`-metodin käyttäminen. Interpolaation käyttäminen tekee koodista yleensä selkeämpää ja lyhyempää.

Käytännössä, Kotlin-kompilaattori korvaa string-interpolaatio ilmaukset niiden arvoilla käännösaikana. Se tarkoittaa, että syntyy tehokasta konekoodia ilman ajonaikaista suorituskyvyn haittaa.

## See Also (Katso Myös):
- [Kotlinin virallinen dokumentaatio merkkijonojen mallintamisesta](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Medium artikkeli Kotlinin string-interpolaatiosta](https://medium.com/@agrawalsuneet/string-interpolation-in-kotlin-4ade1c6f0ed7)
- [Stack Overflow ketju eri tapoista sisällyttää arvoja merkkijonoihin Kotlinissa](https://stackoverflow.com/questions/36523131/how-to-concatenate-strings-in-kotlin)
