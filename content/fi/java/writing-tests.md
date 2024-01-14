---
title:    "Java: Testien kirjoittaminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Testien kirjoittaminen voi vaikuttaa ylimääräiseltä vaivalta, mutta se on tärkeä osa hyvää ohjelmointikäytäntöä. Testit varmistavat, että koodi toimii oikein ja auttavat havaitsemaan mahdolliset virheet ennen kuin ohjelma siirtyy tuotantoon.

## Miten
Testien kirjoittamisen aloittaminen voi vaikuttaa haastavalta, mutta se on todellisuudessa melko yksinkertaista. Alla on kaksi esimerkkiä testien kirjoittamisesta Javalla.

````Java
// Esimerkki testin kirjoittamisesta luokalle, joka laskee kaksi lukua yhteen
public class Laskin {

    public int summaa(int luku1, int luku2) {
        return luku1 + luku2;
    }
}
````

````Java
// Esimerkki testin kirjoittamisesta metodille, joka tarkistaa onko luku parillinen
public class ParillisuusTarkistin {

    public boolean onkoParillinen(int luku) {
        return luku % 2 == 0;
    }
}
````

Testien kirjoittamisessa on tärkeää ottaa huomioon kaikki mahdolliset skenaariot ja varmistaa, että testit kattavat kaikki osat koodista.

## Syvemmälle
Testien kirjoittamalla pystytään takaamaan, että koodi toimii oikein ja vähentämään mahdollisuuksia virheisiin tuotannossa. Lisäksi testien avulla on helpompi löytää ja korjata mahdolliset virheet jo kehitysvaiheessa. On myös tärkeää muistaa, että testien kirjoittaminen auttaa ymmärtämään koodin toimintaa paremmin ja parantaa siten koodin laatua.

## Katso myös
- [JUnit - The Java Unit Testing Framework](https://junit.org/)
- [Test-Driven Development with Java: JUnit Basics](https://www.baeldung.com/junit-basics)
- [How to Write Good Unit Tests: A Beginner's Guide](https://www.freecodecamp.org/news/writing-good-unit-tests-csharpspecflow-basics-best-practices-tips-02287fffaf9/)