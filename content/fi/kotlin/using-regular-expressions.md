---
title:                "Kotlin: Säännöllisten lausekkeiden käyttö"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin tehokkaita työkaluja, jotka auttavat koodaajia löytämään ja manipuloimaan tekstiä halutulla tavalla. Ne ovat erityisen hyödyllisiä, kun joudutaan käsittelemään suuria määriä tekstiä, joka sisältää tiettyjä kaavoja tai malleja. Säännöllisiä lausekkeita käyttämällä voit nopeasti löytää ja muokata tiettyjä kappaleita tai sanoja, mikä tekee työskentelystäsi helppoa ja tehokasta.

## Miten käyttää säännöllisiä lausekkeita Kotlinissa?

Säännöllisen lausekkeen luominen Kotlinissa on melko yksinkertaista. Sinun tarvitsee vain määrittää haluttu teksti ja käyttää säännöllistä lauseketta löytääksesi ja muokataksesi sitä. Esimerkiksi, jos haluat löytää kaikki numerot tekstistä, voit käyttää seuraavaa säännöllistä lauseketta:

```Kotlin
val teksti = "Tässä tekstissä on joitakin numeroita: 123 ja 456."
val regex = Regex("[0-9]+")
val numeroLista = regex.findAll(teksti).map { it.value }.toList()

println(numeroLista) // [123, 456]
```

Kuten näet, käyttämällä säännöllistä lauseketta `[0-9]+`, pystyimme löytämään kaikki numerot tekstistä ja tallentamaan ne listalle. Voit muuttaa säännöllisen lausekkeen kaavoja vastaavasti käyttämällä erilaisia merkkejä ja operaattoreita.

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet seuraavat tiettyjä sääntöjä, kun etsitään tiettyjä kaavoja tai malleja tekstistä. Tämän vuoksi on tärkeää ymmärtää perusteet ennen kuin alat käyttää niitä. Voit esimerkiksi käyttää erilaisia merkkejä, kuten `[]`, `*` ja `+` ilmaisemaan haluttuja kaavoja. On myös olemassa erilaisia modifier- ja escape-merkkejä, jotka voivat auttaa tarkentamaan hakuja.

On tärkeää harjoitella ja tutkia säännöllisiä lausekkeita ennen niiden käyttämistä tuotannossa, jotta ymmärrät täysin niiden toiminnan ja miten voit hyödyntää niitä.

## Katso myös

- [Kotlinin säännöllisten lausekkeiden dokumentaatio](https://kotlinlang.org/docs/regular-expressions.html)
- [Säännöllisten lausekkeiden opas](https://www.regular-expressions.info/)
- [Kotlinsäännöllisten lausekkeiden harjoituksia](https://regexone.com/lesson/introduction_abcs)