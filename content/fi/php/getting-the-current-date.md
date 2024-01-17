---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "PHP: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

"Getting the current date" tarkoittaa päivämäärän hankkimista täsmällisellä tavalla nykyhetken mukaan. Tämä on tärkeää, koska usein ohjelmista tarvitaan tietoa siitä, milloin jokin tiedosto on luotu tai milloin jokin toiminto on suoritettu.

## Miten:

```PHP
<?php 
$currentDate = date('d.m.Y');
echo "Tänään on " . $currentDate;
// Output: Tänään on 14.08.2021
```

## Syvä sukellus:

Päivämäärän hankkiminen on ollut merkittävä osa ohjelmointia jo pitkään, ja se on tärkeä osa monia sovelluksia. Esimerkiksi verkkosivuilla näkyy usein luomis- tai muokkauspäivämäärä uutisartikkeleiden yhteydessä. 

Alternatiivisesti päivämäärän hankkimiseen voisi käyttää myös esimerkiksi JavaScriptin Date-objektia, mutta PHP:n date-funktio on nopeampi ja helpompi tapa saada päivämäärä muodossa, joka halutaan.

Päivämäärän hankkiminen tehdään PHP:ssa date-funktiolla, johon annetaan ensimmäisenä parametrina haluttu päivämäärän muoto ja toisena parametrina nykyhetki. Tämän jälkeen päivämäärä voidaan tulostaa echo-komennolla.

## Katso myös:

- [PHP:n virallinen dokumentaatio date-funktiosta](https://www.php.net/manual/en/function.date.php)
- [JavaScriptin Date-objektin käyttö päivämäärän hankkimiseen](https://www.w3schools.com/jsreF/jsref_getDate.asp)