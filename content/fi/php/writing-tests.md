---
title:                "Testien kirjoittaminen"
html_title:           "PHP: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Miksi testejä kannattaa kirjoittaa? Yksinkertaisesti siksi, että ne takaavat koodin toiminnan ja estävät mahdollisten virheiden syntymisen tuotantoympäristössä. Testien avulla voit myös tehdä muutoksia ja parannuksia koodiin luottavaisin mielin, sillä tiedät että kaikki tärkeimmät toiminnallisuudet on testattu ja toimiviksi todettu.

## Kuinka

Testien kirjoittaminen PHP:ssa voi vaikuttaa haastavalta, mutta se on oikeasti varsin helppoa. Alla on yksinkertainen esimerkki koodista ja sen tuottamasta tuloksesta. Kopioi koodilohkot ja kokeile itse!

```PHP
<?php
require "laskin.php"; // Lataa laskimen toiminnallisuuden

class LaskinTesti extends PHPUnit_Framework_TestCase { // Luo uusi testiluokka
    public function testSummaus() { // Testifunktio summaukselle
        $laskin = new Laskin(); // Luodaan uusi Laskin-olio
        $tulos = $laskin->summaa(5, 3); // Kutsutaan summaa-funktiota
        $this->assertEquals(8, $tulos); // Tarkistetaan, että tulos on oikea
    }
}
```

Tulostus:

```
PHPUnit 5.7.21 by Sebastian Bergmann and contributors.

.

Time: 34 ms, Memory: 4.00MB

OK (1 test, 1 assertion)
```

Testissä käytetään yksinkertaista laskinta, joka on kirjoitettu "laskin.php"-tiedostoon. Testiluokka perii PHPUnit_Framework_TestCase-luokan ja metodi testSummaus suoritetaan testinä. Testissä luodaan uusi Laskin-olio, kutsutaan sen summaa-funktiota ja tarkistetaan, että tulos on odotetunlainen.

## Syvältä

Testien kirjoittaminen on tärkeää, mutta vaatii hieman panostusta. Hyvien testien avulla voit varmistaa koodin toiminnallisuuden, mutta myös helpottaa sen ylläpitoa ja muokkaamista tulevaisuudessa. Joitakin hyviä käytäntöjä testien kirjoittamiseen ovat esimerkiksi:

- Testaa vain yhtä asiaa kerrallaan: pidä testifunktiot mahdollisimman pieninä ja keskity testaamaan vain yhtä asiaa kerrallaan.
- Käytä kuvaavia nimiä: testifunktioiden nimien tulee kertoa selkeästi, mitä ne testaavat.
- Testaa reunatapaukset: varmista, että testit kattavat myös ääritapaukset ja testaavat esimerkiksi virheelliset syötteet.
- Älä pelkää refaktorointia: hyvät testit mahdollistavat koodin jatkokehityksen ja parantamisen ilman pelkoa rikkovansa jotakin. Muista siis päivittää myös testit tarvittaessa.

## Katso myös

- [PHPUnit-dokumentaatio](https://phpunit.de/documentation.html)
- [PHP-virhetestaus PHP Unitillä](https://www.sitepoint.com/getting-started-phpunit/)
- [TDD PHP:ssa - käytännön esimerkkejä](https://www.sitepoint.com/test-driven-development-phpunit/)