---
title:    "Kotlin: Säännöllisten lausekkeiden käyttö"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää tavallisia ilmaisuja (Regular Expressions)?

Tavalliset ilmaisut (tunnetaan myös nimellä regex tai regexp) ovat voimakas työkalu ohjelmoijille, jotka haluavat käsitellä ja muokata merkkijonoja. Ne ovat erityisen hyödyllisiä silloin, kun haluat etsiä tiettyä kaavaa tai sääntöä merkkijonosta.

## Kuinka käyttää tavallisia ilmaisuja?

Tavallisten ilmaisuiden käyttö Kotlinissa on yksinkertaista. Voit käyttää niitä String-luokan matchEntire()-funktiolla tai Regex-luokan oft*-funktioilla. Alla on esimerkki kuinka voit käyttää tavallisia ilmaisuja tarkistamaan, onko annettu merkkijono puhelinnumero:

```Kotlin
val puhelinnumero = "123-456-7890"
val regex = Regex("""\d{3}-\d{3}-\d{4}""")
if (puhelinnumero.matchEntire(regex)) {
    println("Puhelinnumero on oikeassa muodossa!")
} else {
    println("Puhelinnumero ei ole oikeassa muodossa.")
}
```

Tämä koodi tulostaa "Puhelinnumero on oikeassa muodossa!" koska annettu merkkijono täyttää tavallisen ilmaisun säännön.

## Syvällinen sukellus tavallisten ilmaisujen käyttöön

Tavalliset ilmaisut ovat tehokas työkalu merkkijonojen käsittelyyn, mutta niiden käyttö voi olla hieman haastavaa aluksi. Tärkeintä on ymmärtää perussäännöt ja erikoismerkit, joita tavallisissa ilmaisuissa käytetään. Alla on muutamia hyödyllisiä linkkejä, jotka auttavat sinua perehtymään tavallisten ilmaisuiden maailmaan ja hyödyntämään niitä oman koodisi parissa:

- [Kotlinin virallinen dokumentaatio tavallisista ilmaisuista](https://kotlinlang.org/docs/regular-expressions.html)
- [Regex Tester -työkalu, joka auttaa testaamaan ja tarkistamaan tavallisia ilmaisuja](https://regexr.com/)
- [Regex One -verkkokurssi, jossa voit oppia perusteet tavallisista ilmaisuista](https://regexone.com/)

## Katso myös

- [Kotlinin String-luokan dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/) 
- [Kotlindoc -tiedoston ilmaisuja ja säännöllisiä lausekkeita koskevat ohjeet](https://kotlinlang.org/docs/kotlindoc.html#regular-expressions-and-expressions-description-patterns)