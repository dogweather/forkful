---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:38:00.446876-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Päivämäärän jäsentäminen merkkijonosta tarkoittaa merkkijonossa olevan päivämäärän muuttamista tietorakenteeksi, jonka kanssa ohjelma voi operoida. Ohjelmoijat tekevät tämän, koska aika on usein olennainen osa tietoa ja sovelluksessa käytettävien aikatietojen on oltava oikeassa muodossa.

## How to: (Kuinka tehdä:)
```PHP
<?php
$dateString = '2023-04-01 14:00:00';
$parsedDate = date_create_from_format('Y-m-d H:i:s', $dateString);

echo date_format($parsedDate, 'Y-m-d H:i:s'); // Näyttää: 2023-04-01 14:00:00
?>
```
Tämä koodi luo `DateTime`-olion annetun merkkijonon ja formaatin perusteella, jonka jälkeen päivämäärä tulostetaan samassa muodossa.

```PHP
<?php
$dateString = '01.04.2023 14.00.00';
$parsedDate = DateTime::createFromFormat('d.m.Y H.i.s', $dateString);

echo $parsedDate->format('c'); // Näyttää kansainvälisen ISO 8601 päivämäärän ja ajan
?>
```
Toisessa esimerkissä käytetään suomalaista päivämäärämuotoa ja muunnetaan se ISO 8601 standardin mukaiseksi.

## Deep Dive (Syväsukellus):
Päivämäärän jäsentäminen on PHP:ssä muuttunut vuosien varrella. Aikaisemmin päivämääriä käsiteltiin `strtotime` ja `date` funktioilla, mutta nämä eivät olleet kovin joustavia. PHP:n `DateTime` luokka esiteltiin versiossa 5.2.0 ja se toi mukanaan objektilähtöisen tavan työskennellä päivämäärien kanssa, mikä paransi käsittelyn täsmällisyyttä ja luotettavuutta.

Vaihtoehtoisia tapoja jäsentää päivämäärät ovat esimerkiksi `strtotime` funktion käyttäminen, joka yrittää arvailla oikean formaatin:
```PHP
<?php
echo date('Y-m-d', strtotime('01.04.2023')); // Muuttaa Eurooppalaisen päivämäärän Yhdysvaltalaiseen muotoon ja tulostaa: 2023-04-01
?>
```
Mutta, kuten näet, `DateTime` luokka on hallitumpi ja mukautuvampi tapa jäsentää päivämäärät.

Implementaation yksityiskohdat voivat vaikuttaa sovellukseen suorituskyvyn kannalta. Jäsennys voi olla aikaa vievää, jos merkkijonoja on paljon, tai muoto on monimutkainen. On myös tärkeää varmistaa, että käsiteltävä päivämäärämerkkijono on odotetussa muodossa, jotta vältetään jäsentämisvirheet ja sitä myötä ohjelman virheellinen toiminta.

## See Also (Katso Myös):
- PHP:n DateTime-luokan dokumentaatio: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Päivämäärän ja ajan käsittely PHP:ssä: [php.net/manual/en/datetime.format.php](https://www.php.net/manual/en/datetime.format.php)
- Päivämäärän formatointi ja jäsentäminen interaktiivisesti: [php.net/manual/en/datetime.createfromformat.php](https://www.php.net/manual/en/datetime.createfromformat.php)
