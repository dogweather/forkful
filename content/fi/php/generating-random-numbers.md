---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen luominen on prosessi, jossa luodaan numeroita, jotka voivat vaihdella kahden määritellyn arvon välillä. Ohjelmoitsijat käyttävät satunnaislukuja sellaisten asioiden suorittamiseen, kuten testitapausten luominen, simulointien suorittaminen tai satunnaisten tulosten luominen.

## Näin teet:
Käytä PHP: n funktiota random_int() satunnaislukujen luomiseen. Esimerkiksi:

```PHP
<?php
$randomNumber = random_int(0, 100);
echo $randomNumber;
?>
```
Tämä luo satunnaisen kokonaisluvun välillään 0 ja 100, mukaan lukien. Tuloste voi olla mikä tahansa luku näillä rajoilla, esimerkiksi "59".

## Syvä sukellus:
Historiallisesti PHP on käyttänyt funktiota rand() satunnaislukujen luomiseen. Tämä funktio oli kuitenkin huomattavan hidas ja ei tuottanut todella satunnaista tulosta. Tämän vuoksi PHP 7 otettiin käyttöön random_int(), joka on sekä nopeampi että tuottaa enemmän satunnaisia tuloksia.

Vaihtoehtoisesti PHP:ssä voidaan käyttää funktiota mt_rand(), joka on nopeampi kuin rand() mutta ei tuota niin satunnaisia tuloksia kuin random_int(). 

Satunnaislukujen luomiseen PHP:ssä on myös muita monimutkaisempia tapoja, kuten /dev/urandom tai openssl_random_pseudo_bytes(). Nämä tarjoavat kryptografisesti turvallisia satunnaisia lukuja, mutta niiden käyttö on yleensä tarpeetonta ja monimutkaista.

## Katso myös: 
- PHP:n ohjekirja satunnaislukuja varten: [PHP Manual for Random Numbers](https://php.net/manual/en/book.math.php)
- random_int():n virallinen dokumentaatio: [PHP random_int() Function](https://www.php.net/manual/en/function.random-int.php)
- Opi lisää rand():sta, mt_rand():sta ja niiden eroista: [Why mt_rand is better than rand in PHP](https://www.exakat.io/php-likes-random/)
- Kattavampi opas satunnaislukujen luomiseen PHP:ssä: [Generating random numbers in PHP](https://www.sitepoint.com/community/t/generating-random-numbers-in-php/6484)