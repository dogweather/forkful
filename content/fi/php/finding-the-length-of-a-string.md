---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pituuden löytäminen tarkoittaa sen merkkien lukumäärän laskemista. Ohjelmoijat tekevät tämän arvioidakseen tietyn tekstin tai datan pituuden - esimerkiksi, varmistaakseen, että teksti mahtuu tiettyyn tilaan.

## Näin se tehdään:

Seuraava PHP-koodiesimerkki näyttää, kuinka löytää merkkijonon pituus käyttämällä `strlen()` -funktiota:

```PHP
<?php
$merkkijono = 'Hei, maailma!';
$merkkijono_pituus = strlen($merkkijono);
echo $merkkijono_pituus;
?>
```

Tulostuu: `13`

## Syvällä asian ytimessä

Pituuden laskemisella on historiallisia juuria muinaisissa tekstinkäsittelyjärjestelmissä. PHP-kielessä `strlen()` -funktiota käytetään usein; se laskee merkkijonon pituuden olettamalla, että merkkijono koostuu ASCII-merkeistä. Kuitenkin UTF-8-merkkijonoa käytettäessä suositellaan `mb_strlen()` -funktion käyttöä:

```PHP
<?php
$merkkijono = 'Hei, maailma!';
$merkkijono_pituus = mb_strlen($merkkijono, 'UTF-8');
echo $merkkijono_pituus;
?>
```

Tämä on tärkeää, koska `strlen()`-funktio laskee merkkijonon tavujen määrän, ei varsinaisia merkkejä, mikä saattaa aiheuttaa ongelmia monimerkkijärjestelmissä, kuten UTF-8:ssa.

## Katso myös

Lisätietoja merkkijonojen pituuden löytämisestä PHP:ssä voit löytää seuraavista lähteistä:

- [PHP strlen() Function](https://www.w3schools.com/php/func_string_strlen.asp) W3Schools-artikkeli strlen()-funktiosta
- [PHP: strlen - Manual](https://php.net/manual/en/function.strlen.php) PHP:n virallinen manuaali `strlen()`-funktiosta
- [PHP: mb_strlen - Manual](https://php.net/manual/en/function.mb-strlen.php) PHP:n virallinen manuaali `mb_strlen()`-funktiosta.