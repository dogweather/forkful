---
title:    "Kotlin: Testien kirjoittaminen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa testeja?

Testaaminen on tärkeä osa ohjelmointia, joka varmistaa koodin toimivuuden ja luotettavuuden. Kirjoittamalla testeja voit varmistaa, että koodisi toimii halutulla tavalla ja havaita mahdolliset virheet ja bugit ennen kuin ne pääsevät tuotantoon.

## Miten kirjoittaa testeja?

Testien kirjoittaminen Kotlinilla on helppoa ja tehokasta. Käytämme erityisesti kirjastoa nimeltä JUnit, joka tarjoaa valmiit työkalut testien kirjoittamiseen. Seuraavaksi esittelemme, miten voit luoda yksikkötestit yksinkertaiselle funktiolle, joka palauttaa kahden numeron summan.

```Kotlin
fun summa(numero1: Int, numero2: Int): Int {
    return numero1 + numero2
}
```

Nyt voimme luoda testin tälle funktiolle koodista, joka tarkistaa, että summa toimii oikein. Tässä testissä testaamme tapauksia, joissa funktiolle annetaan erilaisia numeroita ja tarkistamme, että palauttaa oikean summan.

```Kotlin
@Test
fun testiSumma() {
    assertEquals(8, summa(5, 3))
    assertEquals(0, summa(-5, 5))
    assertEquals(10, summa(10, 0))
}
```

JUnit tarjoaa `assertEquals`-funktion, joka vertaa odotettua tulosta annettuun tulokseen. Jos odotettu ja saatu tulos eivät täsmää, testi epäonnistuu ja näet virheilmoituksen.

## Syväsukellus testeihin

Testien kirjoittamisessa on tärkeää huolehtia siitä, että testit ovat riittävän kattavia ja testaavat kaikkia mahdollisia skenaarioita. Tällöin voit olla varma, että koodisi toimii halutulla tavalla kaikissa tilanteissa.

Lisäksi testien avulla voit tehdä muutoksia koodiisi turvallisesti. Jos esimerkiksi muutat funktiota, voit ajaa testit uudelleen ja varmistaa, että kaikki toimii edelleen kuten pitääkin. Tämä auttaa estämään mahdollisten uusien virheiden syntymisen.

## Katso myös

- [Kotlinin virallinen dokumentaatio testaamisesta](https://kotlinlang.org/docs/testing.html)
- [JUnitin dokumentaatio](https://junit.org/junit5/docs/current/user-guide/)