---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hahmon yhdistävä merkkien poisto on tiettyjen merkkien kohdennettu poistaminen merkkijonosta esimerkiksi /s, p/, r/p/. Ohjelmoijat tekevät näin usein, kun he haluavat puhdistaa tietoa tai muokata syötteitä.

## Kuinka toimia:

Tässä on yksinkertainen esimerkki PHP:n sisäänrakennetulla `preg_replace()` -funktiolla.

```PHP
<?php
$merkkijono = "Hello Pasters! Tervetuloa. 321.";
$puhdistettu_merkkijono = preg_replace("/[^A-Za-z0-9 ]/", '', $merkkijono);
echo $puhdistettu_merkkijono;
?>
```
Tulos on: "Hello Pasters Tervetuloa 321".

## Syvä sukellus

Hahmon yhdistävä merkkien poisto on ollut olennainen osa ohjelmointia jo vuosikymmenten ajan. Sen juuret ovat varhaisessa tekstinkäsittelyjä keräilijän ohjelmoinnissa, kun tarvittiin tekniikka ei-toivottujen merkkien poistamiseksi.

Vaihtoehtoja `preg_replace()` toiminnalle ovat str_replace(), strtr() tai jopa omatekoisiin toimintoihin pohjautuvat ratkaisut. Valinta riippuu useista tekijöistä, kuten tehtävästä, suorituskyvyn vaatimuksista ja henkilökohtaisista mieltymyksistä.

PHP:n `preg_replace()`toiminto luottaa säännöllisten ilmentymien moottoriin, joka tarjoaa erittäin joustavan ja tehokkaan työkalun monimutkaisiin merkkien käsittelytehtäviin.

## Katso myös:

1. PHP:n virallinen dokumentaatio säännöllisten ilmentymien käsittelystä: http://www.php.net/manual/en/book.pcre.php
2. Stack Overflow -keskustelu PHP:n merkkien poisto -toiminnosta: https://stackoverflow.com/questions/1252693/using-regex-to-remove-comma-from-numbers