---
title:                "Tulostaminen vianjäljitystulosteeksi"
html_title:           "PHP: Tulostaminen vianjäljitystulosteeksi"
simple_title:         "Tulostaminen vianjäljitystulosteeksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnin aikana tarvitsee selvittää, miksi jokin tietty koodi ei toimi odotetusti tai miksi muuttujan arvo ei ole oikea. Tämä ei välttämättä näy ohjelman lopullisessa versiossa, joten käyttöön joudutaan ottamaan debuggaus, joka auttaa selvittämään ongelmien syitä. Tulostamalla debug-tietoja voidaan helposti seurata ohjelman suoritusta ja etsiä mahdollisia virheitä.

## Miten

```PHP
<?php
$muuttuja = "Hei";
print "Muuttujan arvo: $muuttuja";
```
Tulostaa:
```
Muuttujan arvo: Hei
```
PHP:ssä on useita tapoja tulostaa debug-tietoja. Yksi yksinkertainen vaihtoehto on käyttää `print` tai `echo` -komentoa tulostamaan haluttu tieto. Tämä voidaan halutessaan yhdistää muuttujien kanssa käyttämällä muuttujan nimeä ja `$` -merkkiä. Lisäksi voidaan käyttää myös `var_dump()` tai `print_r()` -funktioita, jotka tulostavat tarkempaa tietoa muuttujista ja niiden arvoista. Näiden avulla voidaan esimerkiksi tarkistaa, mitä tietoja toiminto palauttaa tai mitkä arvot muuttujilla on.

## Syvällinen tarkastelu

Tulostettaessa debug-tietoja, on tärkeää ottaa huomioon myös muut seikat, kuten ohjelman suoritusnopeus ja tietoturva. Huolimaton debug-tulostus voi hidastaa ohjelman suoritusta ja jopa altistaa sen haavoittuvuuksille. Siksi on hyvä käyttää esimerkiksi `error_reporting(0);` -komentoa, joka estää PHP:tä tulostamasta virheitä sivulle ja hidastamasta suoritusta. Lisäksi on hyvä muistaa poistaa kaikki debug-tulostukset lopullisesta versiosta ennen sen julkaisemista.

## Katso myös

- [PHP manuaali](https://www.php.net/manual/en/)
- [Debuggaus PHP:n avulla](https://www.tutorialspoint.com/php/php_debugging_techniques.htm)
- [PHP Debug Bar](https://phpdebugbar.com/)