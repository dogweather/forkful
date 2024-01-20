---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Tekstin hakeminen ja korvaaminen on menetelmä, jolla löydämme ja muutamme koodeihin upotettua tekstiä. Ohjelmoijat tekevät tämän korjatakseen virheitä, päivittääkseen tietoja tai selkeyttääkseen koodia.

## Näin se tehdään:

```PHP
<?php
$alkuperainen_teksti = "Tervetuloa, Talo!";
$uusi_teksti = str_replace("Talo", "Koti", $alkuperainen_teksti);
echo $uusi_teksti;
?>
```
Yllä oleva pätkä löytää sanan "Talo" alkuperäisestä tekstistä ja korvaa sen sanalla "Koti". Ohjelman ajamisen jälkeen saamme tulostuksen "Tervetuloa, Koti!".

## Syväsukellus

1. Historiallinen konteksti: PHP:n `str_replace()` -funktio on ollut käytössä PHP 4 -versiosta lähtien, mikä julkaistiin vuonna 2000. Se on yksinkertainen ja tehokas tapa manipuloida merkkijonoja.

2. Vaihtoehdot: On muitakin funktioita, kuten `preg_replace()`, jotka käyttävät säännöllisiä lausekkeita hakemiseen ja korvaamiseen. Se on tehokkaampi, mutta monimutkaisempi.

3. Toteutuksen yksityiskohdat: `str_replace()` -funktio skannaa merkkijonon ja korvaa jokaisen löydetyn kohteen. Jos korvattavaa tekstiä ei löydy, toiminto palauttaa alkuperäisen merkkijonon.

## Katso myös:

- [PHP str_replace()](https://www.php.net/manual/en/function.str-replace.php) - virallinen PHP-dokumentaatio `str_replace()` -funktiosta
- [PHP preg_replace()](https://www.php.net/manual/en/function.preg-replace.php) - virallinen PHP-dokumentaatio `preg_replace()` -funktiosta