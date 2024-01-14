---
title:                "Kotlin: Jonojen yhdistäminen"
simple_title:         "Jonojen yhdistäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja ohjelmoidessasi? Usein on tarpeen luoda dynaamisia merkkijonoja, jotka vaihtelevat käyttäjän syöttämien tietojen tai erilaisten muuttujien perusteella. Tämä tekee ohjelmasta monipuolisemman ja antaa mahdollisuuden esimerkiksi personoida viestejä tai luoda yksilöllisiä tulosteita.

## Näin teet

Onneksi Kotlinilla on helppo ja selkeä tapa yhdistää merkkijonoja. Voimme käyttää plus-merkkiä (+) yhdistäessämme kaksi merkkijonoa. Alla on esimerkki siitä, kuinka voit yhdistää käyttäjän nimen ja tervehdyksen:

```Kotlin
val nimi: String = "Matti"
val tervehdys: String = "Hei, "
val tulos: String = nimi + ", " + tervehdys
println(tulos)
```

Tämä tulostaisi "Hei, Matti". Voit myös yhdistää useamman merkkijonon samassa lauseessa käyttämällä plus-merkkiä jokaisen merkkijonon välissä.

## Syvemmälle

Kotlinissa merkkijonojen yhdistämistä kutsutaan concatenationiksi. Tärkeä huomioitava asia on, että merkkijonan yhdistäminen plus-merkillä käynnistää taustalla uuden StringBuilder-olion luomisen ja sen jälkeen merkkijonan liittämisen siihen. Tämä tarkoittaa, että jos yhdistät suuren määrän merkkijonoja, se voi hidastaa ohjelman suorituskykyä. Tästä syystä suositellaankin käyttämään mieluummin StringBuilder-luokkaa, joka on optimoitu merkkijonojen yhdistämistä varten. Voit käyttää sitä seuraavasti:

```Kotlin
val stringBuilder = StringBuilder()
stringBuilder.append("Ensimmäinen merkkijono")
stringBuilder.append("Toinen merkkijono")
println(stringBuilder.toString())
```

Tämä lähestymistapa on tehokkaampi ja nopeampi, mutta suhteellisen samanlainen kuin merkkijonojen yhdistäminen plus-merkillä. Lisäksi StringBuilderilla on muita hyödyllisiä metodeja, kuten insert() ja replace(), joilla voit muokata merkkijonoja ennen niiden yhdistämistä.

## Katso myös

- [Kotlinin merkkijonojen manipulointi](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [StringBuilder-luokka](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- [Kotlinin virallinen verkkosivusto](https://kotlinlang.org/)