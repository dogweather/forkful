---
title:    "Kotlin: Virhetulostuksen tulostaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein koodin kehittämisen yhteydessä törmäämme erilaisiin virheisiin ja ongelmakohtiin. Yksi tapa selvittää näitä ongelmia on käyttää "debug output" -tulostusta, joka auttaa meitä ymmärtämään, mitä koodi oikeasti tekee ja miksi tietty virhe tapahtuu.

## Miten

Debug output -tulostusta voidaan käyttää Kotlinissa yksinkertaisesti println()-funktiolla. Tämä tulostaa halutun muuttujan arvon tai tekstin terminaaliin. Esimerkiksi:

```Kotlin
var x = 5
println(x)
```

Tämä tulostaisi terminaaliin "5". Voit myös käyttää tulostukseen useampia muuttujia tai tekstiä yhdistämällä niitä plussa-merkillä:

```Kotlin
var x = 5
var y = 10
println("x:n arvo on " + x + " ja y:n arvo on " + y)
```

Tämä tulostaisi "x:n arvo on 5 ja y:n arvo on 10" terminaaliin.

## Syvempi sukellus

Debug output -tulostus voi olla erittäin hyödyllinen työkalu koodin kehittämisessä ja virheiden etsimisessä. Kannattaa kuitenkin muistaa, että joskus tulostamalla kaikki mahdolliset muuttujat ja tekstit, koodistamme saattaa tulla hyvin epäselvää ja vaikeasti luettavaa. Siksi on tärkeää harkita tarkasti, mitä haluamme tulostaa ja missä vaiheessa koodia.

Voit myös käyttää Kotlinin logging-kirjastoa, joka tarjoaa monipuolisempia mahdollisuuksia debug output -tulostukseen. Tämä kirjasto tarjoaa esimerkiksi eri tasoisia lokitiedostoja, joihin voit tallentaa tiettyjä virheilmoituksia ja tarkastella niitä myöhemmin.

## Katso myös

- [Kotlinin virallinen dokumentaatio debug output -tulostuksesta](https://kotlinlang.org/docs/reference/basic-types.html#print-and-debug)
- [Kotlinin logging-kirjasto](https://github.com/Kotlin/kotlin-logging)