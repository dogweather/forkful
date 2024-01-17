---
title:                "Testien kirjoittaminen"
html_title:           "Java: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

Mitä ja Miksi?
Testien kirjoittaminen tarkoittaa ohjelmakoodin suorituskyvyn ja oikeellisuuden tarkistamista. Ohjelmoijat tekevät sitä varmistaakseen, että heidän koodinsa toimii odotetulla tavalla ja löytääkseen mahdollisia virheitä jo ennen kuin ohjelma pääsee käyttäjien käsiin.

Kuinka:
Java-testien kirjoittamiseksi tarvitset JUnit-kirjaston, jonka avulla voit tehdä automatisoituja testejä Java-ohjelmalle. Seuraavassa esimerkissä luodaan yksinkertainen testi, joka tarkistaa, että kaksi lukua lisätään oikein:

```Java
import static org.junit.Assert.assertEquals;

public class CalculatorTest {

    @Test
    public void addTest() {
        Calculator calc = new Calculator();
        int result = calc.add(2, 2);
        assertEquals(4, result);
    }
}
```

Tämä testi luo uuden laskimen ja käyttää sen add-metodia syöttäen sille kaksi lukua. Sen jälkeen se verrataan odotettuun tulokseen (4) ja jos ne ovat samat, testi onnistuu. On tärkeää, että testit ovat riippumattomia toisistaan ja niiden tulisi kattaa mahdollisimman paljon koodia.

Syvä sukellus:
JUnit-kirjasto kehitettiin alun perin vuonna 1998 ja on tähän päivään asti yksi suosituimmista testauskehyksistä Java-sovelluksille. Siinä on myös mahdollisuus käyttää JUnit Jupiter API:a, joka tuo mukanaan uusia ominaisuuksia, kuten parametrilliset testit ja lamba-lausekkeet. On myös muita vaihtoehtoja, kuten TestNG, joka tarjoaa vastaavia toiminnallisuuksia.

Testien kirjoittamiseen on monia erilaisia lähestymistapoja ja strategioita, mutta tärkeintä on löytää sopiva tapa tiimillesi ja projektisi tarpeisiin. Testikattavuuden lisäksi on myös tärkeää miettiä, mikä on sopiva määrä testaukselle - liikaa testejä voi hidastaa kehitystä.

Katso myös:
- JUnit: https://junit.org/junit5/
- TestNG: https://testng.org/doc/index.html