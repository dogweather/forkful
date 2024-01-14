---
title:    "Kotlin: Viimeistäminen merkkijonoja"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Miksi: Miksi yhdistää merkkijonoja?

Merkkijonojen yhdistäminen on tärkeä osa ohjelmointia, koska se mahdollistaa erilaisten tietojen yhdistämisen yhdeksi kokonaisuudeksi. Esimerkiksi voit yhdistää etunimen ja sukunimen yhteen merkkijonoon luodaksesi kokonaisen nimen. Tämä helpottaa tietojen käsittelyä ja esittämistä käyttäjälle.

## Kuinka: Esimerkkejä koodilla ja tulosteilla

Yhdistäminen onnistuu helposti Kotlinissa käyttämällä "plus" merkkiä (+) merkkijonojen välissä. Tässä esimerkissä yhdistämme etu- ja sukunimen yhteen ja tulostamme lopputuloksen konsoliin:

```Kotlin
val etunimi = "Matti"
val sukunimi = "Meikäläinen"
val kokonimi = etunimi + sukunimi
println(kokonimi)
```

Tuloste:

```Kotlin
MattiMeikäläinen
```

Voit myös yhdistää merkkijonoja suoraan tulostamisen yhteydessä. Tässä esimerkissä tulostamme tervehdyksen, jossa käytämme merkkijonojen yhdistämistä:

```Kotlin
val etunimi = "Liisa"
println("Hei $etunimi, tervetuloa!")
```

Tuloste:

```Kotlin
Hei Liisa, tervetuloa!
```

## Syvällinen tarkastelu: Yhdistämisen detaljit

Kotlinissa on mahdollista yhdistää eri tyyppisiä tietoja, kuten merkkijonoja ja muuttujia, käyttämällä "plus" merkkiä (+). Tämä tekee koodista joustavaa ja helppolukuista. Huomaa myös, että Kotlinissa on mahdollista yhdistää useita merkkijonoja kerralla. Esimerkiksi:

```Kotlin
val tulostus1 = "Hello"
val tulostus2 = "World!"
val yhdistetty = tulostus1 + tulostus2
println(yhdistetty)
```

Tuloste:

```Kotlin
Hello World!
```

Voit myös käyttää "plus" merkkiä yhdistääksesi merkkijonoja muihin datatyyppisiin, kuten numeromuuttujiin. Kotlin hoitaa automaattisesti muunnoksen merkkijonoksi. Esimerkiksi:

```Kotlin
val numero = 42
val teksti = "Vastaus on: " + numero
println(teksti)
```

Tuloste:

```Kotlin
Vastaus on: 42
```

Yhdistämisen lisäksi Kotlinissa on myös muita tapoja manipuloida merkkijonoja, kuten vaihtaa niiden järjestystä, poistaa tai lisätä merkkejä sekä muuttaa ne isoihin tai pieniin kirjaimiin. Kannattaa kokeilla erilaisia vaihtoehtoja ja löytää itselleen sopivin tapa käsitellä merkkijonoja.

## Katso myös

- [Kotlinin virallinen dokumentaatio merkkijonojen yhdistämisestä.](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlinin String-interpolation lisätietoja merkkijonojen yhdistämisestä.](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Ohjelmointiopas: Merkkijonojen yhdistäminen ja manipulointi Kotlinissa.](https://programiz.com/kotlin-programming/string)