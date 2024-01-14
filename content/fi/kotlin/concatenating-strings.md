---
title:    "Kotlin: Merkkijonojen yhdistäminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on tärkeä osa ohjelmointia, sillä se mahdollistaa erilaisten tiedon esittämisen yhtenä kokonaisuutena. Se on erityisen hyödyllistä silloin, kun halutaan tulostaa tekstiä, jossa on dynaamisia osia, kuten muuttuvia muuttujia.

## Miten tehdä

Stringien yhdistäminen on helppoa Kotlinissa. Se voidaan tehdä käyttämällä plus-merkkiä (`+`) kahden merkkijonon välissä, tai käyttämällä `.plus()`-funktiota.

```Kotlin
val etunimi = "Maija"
val sukunimi = "Meikäläinen"

val nimi = etunimi + sukunimi
val tervehdys = "Hei, " + etunimi + " " + sukunimi + "!"

println(nimi) // tulostaa "MaijaMeikäläinen"
println(tervehdys) // tulostaa "Hei, Maija Meikäläinen!"
```

## Syvällinen sukellus

Kotlinissa merkkijonojen yhdistäminen tehdään tehokkaasti taustalla käyttämällä `StringBuilder`-luokkaa. Tämä tarkoittaa sitä, että merkkijonon yhdistämisen suorituskyky on erittäin hyvä ja tehokas. Se myös mahdollistaa merkkijonojen yhdistämisen ketjutetusti, eli useamman merkkijonon yhdistämisen yhdellä kertaa.

```Kotlin
val alkusanat = "Olen" + " " + "Maija" + "."
val tervehdys = "Hei,".plus(" " + alkusanat).plus("Mukava nähdä sinua!")

println(tervehdys) // tulostaa "Hei, Olen Maija. Mukava nähdä sinua!"
```

## Katso myös

- [Official Kotlin documentation on String concatenation](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation)
- [Java String Builder vs String Concatenation](https://stackoverflow.com/questions/15365227/java-stringbuilder-vs-string-concatenation)