---
title:                "Kotlin: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testaamisen kirjoittaminen voi tuntua turhalta ja aikaa vievältä työltä, mutta se on todella tärkeä osa ohjelmistokehitystä. Testien avulla voimme varmistaa, että koodimme toimii halutulla tavalla ja vähentää virheiden riskiä tuotantoympäristössä.

## Miten

Testien kirjoittaminen Kotlinilla on helppoa ja suoraviivaista. Voit käyttää sisäänrakennettua `assert()` -funktiota testien kirjoittamiseen. Katso alla oleva esimerkki `SimpleCalculator` -luokan testaamisesta:

```Kotlin
class SimpleCalculator {
    fun add(x: Int, y: Int) = x + y
}

fun main() {
    val calc = SimpleCalculator()
    assert(calc.add(3, 4) == 7)
    assert(calc.add(-5, 10) == 5)
    println("All tests passed!")
}
```

```
All tests passed!
```

Kuten näet, voimme käyttää assert-funktiota testaamaan, onko laskinluokan `add()` -metodi palauttaa odotetun tuloksen erilaisilla lähtöarvoilla.

Voit myös käyttää ulkoista kirjastoa, kuten JUnit, helpottamaan testien kirjoittamista ja suorittamista.

## Syvällinen sukellus

Testien kirjoittaminen on tärkeää myös sovelluksen jatkokehityksen kannalta. Kun uusia toiminnallisuuksia lisätään tai vanhoja muokataan, testien avulla voimme varmistaa, ettei muutoksista aiheudu odottamattomia virheitä vanhoissa toiminnallisuuksissa.

Lisäksi testien kirjoittaminen auttaa myös koodin toiminnallisuuden ymmärtämisessä ja parantaa siten ohjelman laatua ja ylläpidettävyyttä.

## Katso myös

- [Kotlinin testaamisen perusteet](https://www.fi.kotlinlang.org/docs/tutorials/testing.html)
- [JUnit-kirjaston käyttö Kotlinilla](https://www.baeldung.com/junit-5-kotlin)