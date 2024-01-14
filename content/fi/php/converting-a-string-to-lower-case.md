---
title:    "PHP: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi ihmisten kannattaa muuttaa merkkijono pieniksi kirjaimiksi? Yksinkertaisesti sanottuna, muuttamalla merkkijonon pieniksi kirjaimiksi varmistat, että kirjainkoolla ei ole merkitystä. Tämä on erittäin hyödyllistä esimerkiksi käyttäjänimen tai salasanan tarkistamisessa, jotta käyttäjät voivat kirjoittaa ne eri tavoin, mutta ne rekisteröityvät silti samana.

## Kuinka tehdä

PHP: ssä on helppo muuttaa merkkijono pieniksi kirjaimiksi käyttämällä sisäänrakennettuja toimintoja strtolower () tai mb_strtolower (). Kumpikin näistä palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat pieniä. Katso alla olevat esimerkit ja tulostaulukot.

```PHP
$string = "Tämä on TESTImerkkijono";
echo strtolower($string);
// Tulostaa "tämä on testimerkkijono"

$string = "TÄMÄ ON ÆÖÅ TESTI";
echo mb_strtolower($string);
// Tulostaa "tämä on æöå testi"
```

## Syvälle sukeltaminen

On tärkeää huomata, että strtolower () ja mb_strtolower () toimivat vain kirjaimilla, jotka on määritelty merkkijonossa. Kaikki numerot tai erikoismerkit, kuten !, @ tai & pysyvät samana. Myös romaaniset merkit, kuten ß tai á, muutetaan vastaaviin latinalaisiin kirjaimiin, kuten ss tai a. Tämä voi aiheuttaa ongelmia, jos merkkijonoa käytetään esimerkiksi salasanassa, jossa merkkien koko on tärkeä. Tämä on asia, joka on otettava huomioon merkkijonoja muuttaessa pieniksi kirjaimiksi.

## Katso myös

- PHP virallinen dokumentaatio strtolower (): https://www.php.net/manual/en/function.strtolower.php
- PHP virallinen dokumentaatio mb_strtolower (): https://www.php.net/manual/en/function.mb-strtolower.php
- PHP-funktioiden vertailu: strtolower () vs mb_strtolower (): https://www.techiediaries.com/php-strtolower-vs-mb_strtolower/