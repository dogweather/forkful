---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Aliketjujen erottaminen on prosessi, jossa valitaan ja kopiodaan merkkijonoista osia uuteen merkkijonoon. Ohjelmoijat tekevät tämän, koska se auttaa pitämään koodin siistinä, helpottaa tietojen käsittelyä ja analysointia.

## Kuinka:

Käytämme PHP:n sisäänrakennettua `substr()` -funktiota. Tässä esimerkki:

```PHP
<?php
$input = "Hello, World!";
$result = substr($input, 7, 5);
echo $result;
?>
```

Tulostettu merkkijono on "World".

## Syvä Sukellus:

Aikaisemmissa PHP-versioissa oli olemassa funktio nimeltä `substr()`, joka toimi samalla tavalla mutta oli hitaampi. PHP 7 paransi tämän. Vaihtoehtoisesti voit käyttää `mb_substr()` -funktiota, jos käsittelet monitavuisia merkkijonoja.

PHP:n `substr()` -funktion toteutus on tehokas, tapa, jolla se käsittelee merkkijonoja. Se luo uuden merkkijonon, jossa on sama alkuperäinen merkkijono mutta eri aloitus- ja lopetuspisteet. Tämä tarkoittaa, että `substr()` muistuttaa 'viittauksia' alkuperäiseen merkkijonoon eikä kopioi merkkijonoa, mikä tekee siitä nopean.

## Katso Myös:

- [PHP:n virallinen dokumentaatio substr-funktiosta](https://www.php.net/manual/en/function.substr.php)
- [PHP The Right Way - suositukset ja parhaat käytännöt](https://phptherightway.com)