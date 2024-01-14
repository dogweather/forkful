---
title:                "Kotlin: Ohjelmointitestien kirjoittaminen"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testien kirjoittaminen voi tuntua aikaa vievältä ja turhalta askareelta, mutta ne ovat erittäin tärkeä osa ohjelmistokehitystä. Hyvin kirjoitetut testit voivat havaita mahdollisia virheitä ja parantaa ohjelman suorituskykyä. Lisäksi ne auttavat uusien kehitysten helpossa ja turvallisessa integroimisessa olemassa olevaan koodiin.

## Miten

Kotlinin avulla testien kirjoittamisesta voi tulla paljon helpompaa ja tehokkaampaa. Seuraavassa on esimerkki yksinkertaisista testiluokista, jotka testaavat funktiota, joka laskee kahden numeron summan.

```
Kotlin
class SumTest {
    @Test
    fun `sum function returns correct result`() {
        val result = sum(5, 10)
        assertEquals(15, result)
    }

    @Test
    fun `sum function returns incorrect result`() {
        val result = sum(7, 3)
        assertEquals(10, result)
    }
}

fun sum(num1: Int, num2: Int): Int {
    return num1 + num2
}
```

Testiluokan voi luoda käyttämällä `class`-avainsanaa ja antamalla sille nimen, esim. `SumTest`. Sen jälkeen testit voidaan kirjoittaa käyttämällä `@Test`-annotaatiota ja antamalla niille kuvaava nimi, kuten `sum function returns correct result`. Tässä annotaatiossa testifunktio on määritelty takapuoletuksena, joten sen voi jättää tyhjäksi. `assertEquals`-funktiolla voi varmistaa, että odotettu tulos ja testin palauttama tulos ovat samat.

## Syväsukellus

Testien kirjoittaminen on tärkeä osa ketterää ohjelmistokehitystä, sillä se auttaa havaitsemaan mahdolliset ongelmat ja varmistamaan ohjelman luotettavuuden. Hyvin kirjoitetut testit tarjoavat myös dokumentaatiota ohjelman toiminnallisuudesta ja osoittavat, että koodi on testattu ja toimii odotetulla tavalla.

On tärkeää muistaa, että testien kirjoittamiseen kannattaa käyttää riittävästi aikaa ja huolellisuutta, sillä huonosti kirjoitetut testit voivat aiheuttaa enemmän haittaa kuin hyötyä. On myös hyvä käyttää erilaisia testauksen työkaluja ja menetelmiä, kuten yksikkötesteissä mock-olioita ja integraatiotesteissä simuloituja tietokantoja.

## Katso myös

- [Testien kirjoittaminen Kotlinilla](https://kotlinlang.org/docs/testing.html)
- [Kotlin-testikirjaston käyttöönotto Gradle-projektissa](https://ktor.io/quickstart/gradle.html)
- [Kattava opas yksikkötestaamiseen Kotlinilla](https://proandroiddev.com/kotlin-android-test-driven-development-with-kotlintest-mockk-and-spek-1e01a8ecf8fe)