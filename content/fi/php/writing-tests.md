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

## Mitä & Miksi?

Testaaminen on prosessi, jossa koodia tarkistetaan sen toiminnallisuuden ja virheettömyyden varmistamiseksi. Koodin testaaminen on tärkeä osa ohjelmointia, joka auttaa varmistamaan, että sovellus toimii oikein ja vähentää mahdollisten virheiden riskiä.

## Miten:

```PHP
<?php
//Luodaan testi-luokka
class Testi extends PHPUnit_Framework_TestCase
{
  //Testataan yhteenlaskua
  public function testYhteenlasku()
  {
    $luku1 = 5;
    $luku2 = 10;
    $tulos = $luku1 + $luku2;
    //Asserttestilla varmistetaan, että tulos on oikein
    $this->assertEquals(15, $tulos);
  }
}
```

Testin suorittamisen jälkeen saat odotetun tulosteen ilman virheitä:

```
OK (1 testi, 1 testattu)
```

## Syväsukellus:

Testien kirjoittamisessa on kaksi yleistä lähestymistapaa: yksikkötestaus ja integraatiotestaus. Yksikkötestauksessa testataan yksittäisiä toiminnallisuuksia ja niiden algoritmeja, kun taas integraatiotestauksessa testataan komponenttien yhteistoimivuutta.

Muita ohjelmistotestauksen muotoja ovat esimerkiksi manuaaliset testit tai automatisointityökalujen käyttö. PHP:n lisäksi on mahdollista kirjoittaa testejä myös muilla ohjelmointikielillä, kuten JavaScriptillä tai Pythonilla.

Testien kirjoittaminen voi myös auttaa tunnistamaan ja korjaamaan virheitä koodissa. Kannattaa myös muistaa, että testit eivät korvaa huolellista suunnittelua ja koodauksen tarkistamista.

## Katso myös:

- [PHPUnit dokumentaatio](https://phpunit.de/manual/current/en/writing-tests-for-phpunit.html)
- [PHP:n sisäänrakennettu testaus](https://www.php.net/manual/en/intro.pdo.php)