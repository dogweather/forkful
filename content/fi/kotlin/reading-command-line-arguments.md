---
title:                "Kotlin: Komentoriviparametrien lukeminen."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentorivin argumentteja?

Kun ohjelmoit Kotlinilla, on tärkeää ymmärtää, miten voit lukea komentorivin argumentteja. Tämä antaa sinulle mahdollisuuden muokata ohjelmaasi suoraan komentoriviltä, mikä on erittäin hyödyllistä testauksessa ja virheiden korjaamisessa. Se on myös yksi perustavaa laatua olevista taidoista, kun opetellaan ohjelmointia.

## Kuinka lukea komentorivin argumentteja Kotlinilla

Voit helposti lukea komentorivin argumentteja Kotlinissa käyttämällä `args`-muuttujaa. Tämä muuttuja sisältää taulukon kaikista komentorivin argumenteista. Voit tulostaa ne konsolille yksinkertaisesti käyttämällä `println`-funktiota.

```Kotlin
fun main(args: Array<String>) {
    println(args.joinToString())
}
```

Tämä tulostaa kaikki komentorivin argumentit yhdistettyinä merkkijonoksi. Voit myös käsitellä erilaisia argumentteja erikseen taulukkoindeksien avulla.

```Kotlin
fun main(args: Array<String>) {
    println(args[0]) // ensimmäinen komentorivin argumentti
    println(args[1]) // toinen komentorivin argumentti
}
```

Voit myös käyttää `args`-muuttujaa muiden funktioiden ja ehtojen sisällä, esimerkiksi luodaksesi ehtolausekkeen, joka tarkistaa, onko tietty argumentti annettu.

## Syvempää tietoa komentorivin argumenttien lukemisesta

Kun luet komentorivin argumentteja Kotlinilla, on tärkeää tietää, että `args`-muuttuja sisältää aina vähintään yhden arvon: ohjelman nimet. Tämä tarkoittaa, että jos et anna mitään argumentteja, taulukko on silti kooltaan vähintään yksi.

On myös tärkeää muistaa, että komentorivin argumentit on eroteltu välilyönneillä. Tämä tarkoittaa, että jos haluat lukea argumentin, joka sisältää välilyönnin, sinun on yhdistettävä useampi taulukon indeksi yhteen merkkijonoon.

Toinen hyödyllinen vinkki on käyttää `args`-muuttujan `size`-ominaisuutta, joka kertoo taulukon koon. Tämän avulla voit luoda silmukan, joka lukee kaikki komentorivin argumentit riippumatta siitä, kuinka monta niitä on.

## Katso myös

- [Kotlinin perusteiden oppiminen](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Kotlinin Array- ja List-tietorakenteiden käyttö](https://kotlinlang.org/docs/reference/basic-types.html#arrays)

Kirjoittaja: [Sofia Laakso](https://github.com/sofialaakso)