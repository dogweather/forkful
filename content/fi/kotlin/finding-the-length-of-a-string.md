---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Merkinjonon (tai "stringin") pituus on siinä olevien merkkien määrä. Ohjelmoijat tarvitsevat tätä tietoa usein esimerkiksi validoidessaan käyttäjän syötteitä tai käsitellessään tekstidataa.

## Kuinka:

Kotlinin 'String' -luokassa on 'length' -ominaisuus, joka palauttaa jonojen pituudet.

```Kotlin
val merkkijono = "Hei Maailma"
println("Merkkijonon pituus: ${merkkijono.length}")
```

Edellä oleva esimerkki ilmoittaa "Merkkijonon pituus: 11", koska merkkijonossa "Hei Maailma" on 11 merkkiä.

## Syvä sukellus:

Historiallisesti merkkijonojen pituuksien laskeminen on ollut osa ohjelmointia alusta alkaen. Se on yleinen toiminto useimmissa ohjelmointikielissä, mukaan lukien Kotlinin edeltäjät, kuten Java.

Vaihtoehtona yksittäisten merkkijonojen pituuden laskemiselle voit käyttää 'map' ja 'length' -funktioita laskemaan useiden merkkijonojen pituudet samanaikaisesti. 

```Kotlin
val merkkijonot = listOf("sinä", "minä", "me")
val pituudet = merkkijonot.map { it.length }
println(pituudet) // Tulostaa: [4, 4, 2]
```

Tämä lähestymistapa hyödyntää Kotlinin funktionaalista ohjelmointipuolta ja tehostaa useiden merkkijonojen käsittelyä.

Kotlinin 'length' -ominaisuus pilkkoo merkkijonon UTF-16-koodiyksiköihin ja laskee nämä yksiköt, mikä tarkoittaa, että se ei aina kuvasta todellista merkkien määrää, jos merkkijonossa on nelibittisiä Unicode-merkkejä.

## Katso myös:

Lisätietoja ja liittyviä aiheita:

- Kotlinin virallinen dokumentaatio: [String -koko](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- Merkkijonofunktiot Kotlinilla: [Kotlin String Functions](https://www.programiz.com/kotlin-programming/string)
- UTF-16 selitetty: [UTF-16 - Wikipedia](https://fi.wikipedia.org/wiki/UTF-16)