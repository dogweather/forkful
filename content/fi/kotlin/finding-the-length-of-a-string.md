---
title:                "Kotlin: Merkkijonon pituuden löytäminen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Kotlin on monikäyttöinen ja helposti opittava ohjelmointikieli, joka on tullut suosituksi erityisesti Android-sovellusten kehittämisessä. Yksi tärkeimmistä taidoista ohjelmointikielen hallitsemisessa on kyky muokata ja käsitellä merkkijonoja. Merkkijonojen pituuden laskeminen on yksi näistä taidoista, joka on hyödyllinen monissa ohjelmoinnin tehtävissä.

## Miten?

Merkkijonojen pituuden laskeminen Kotlinissa on helppoa. Voit käyttää `length` -funktiota, joka palauttaa merkkijonon pituuden. Alla on esimerkki:

```Kotlin
val sana = "Tervetuloa"
println(sana.length) //tulostaa 10
```

Voit myös laskea merkkijonojen pituuden käyttämällä `count` -funktiota. Se palauttaa merkkijonon merkkien määrän ainoastaan, eikä huomioi esimerkiksi välilyöntejä. Alla on esimerkki:

```Kotlin
val lause = "Tämä on esimerkkilause"
println(lause.count()) //tulostaa 22
```

Voit myös käyttää `length` ja `count` yhdessä saadaksesi merkkien ja välilyöntien lukumäärän. Esimerkiksi:

```Kotlin
val lause = "Tämä on esimerkkilause"
val pituus = lause.length + lause.count()
println(pituus) //tulostaa 24
```

## Syvemmälle

Kotlinin merkkijonojen pituuden laskeminen perustuu `CharSequence` -rajapintaan, joka antaa pääsyn merkkijonon elementteihin ja niiden määrään. Tämä tarkoittaa sitä, että merkkijonojen pituuden laskeminen ei riipu pelkästään `length` tai `count` -funktioista, vaan voit myös käyttää muita `CharSequence` -rajapinnan tarjoamia toimintoja.

Merkkijonojen pituuden laskeminen voi myös olla hyödyllistä silloin, kun haluat esimerkiksi rajata käyttäjän antaman merkkijonon tiettyyn pituuteen, tai tarkistaa onko merkkijono riittävän pitkä ennen sen käyttämistä.

## Katso myös

- [Kotlinin merkkijonojen käsittely](https://kotlinlang.org/docs/strings.html)
- [Kotlinin `CharSequence` -rajapinta](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-char-sequence/)