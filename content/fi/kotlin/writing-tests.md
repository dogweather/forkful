---
title:    "Kotlin: Testien kirjoittaminen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Testit ovat tärkeä osa ohjelmoinnin prosessia, ja ne auttavat varmistamaan, että koodi toimii ja toimii oikein. Ne myös auttavat havaitsemaan virheitä ja puutteita koodissa ennen kuin se päätyy tuotantoon. Kirjoittamalla testejä voit parantaa koodisi laatua ja vähentää virheiden määrää.

## Miten

Testien kirjoittaminen Kotlinissa on helppoa ja intuitiivista. Tässä esimerkissä luomme yksinkertaisen funktion, joka tarkistaa, onko annettu luku parillinen vai ei:

```Kotlin
fun tarkistaParillinen(numero: Int): Boolean {
    return numero % 2 == 0
}
```

Voit sitten lisätä tämän funktion yksikkötestiin käyttämällä JUnit-kirjastoa:

```Kotlin
@Test
fun testiTarkistaParillinen() {
    assertEquals(tarkistaParillinen(2), true)
}
```

Voit myös luoda testiluokan ja ajaa kaikki testit yhdellä kertaa:

```Kotlin
class Testiluokka {

    @Test
    fun testiTarkistaParillinen() {
        assertEquals(tarkistaParillinen(2), true)
    }

    @Test
    fun testiTarkistaParittomia() {
        assertEquals(tarkistaParillinen(3), false)
    }
}
```

Tämä on vain yksinkertainen esimerkki testien kirjoittamisesta, mutta voit luoda monimutkaisempia testejä riippuen koodisi tarpeista. Muista myös dokumentoida testit kattavasti, jotta ne olisivat ymmärrettäviä ja helppoja ylläpitää.

## Syväsukellus

Kotlinissa voit myös käyttää muita testikirjastoja, kuten Spek ja TestNG, riippuen mieltymyksistäsi ja projektisi vaatimuksista. Voit myös ottaa käyttöön Mockito-kirjaston, joka auttaa luomaan yksikkötesteja komponenteille, jotka ovat riippuvaisia muista luokista.

On myös tärkeää muistaa, että testien kirjoittaminen ei korvaa laadukasta koodausta, vaan se on tärkeä lisä siihen. Testien kirjoittaminen auttaa sinua varmistamaan, että koodisi on toimivaa ja vähentää odottamattomien virheiden riskiä.

## Katso myös

- [JUn