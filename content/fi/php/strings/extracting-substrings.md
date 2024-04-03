---
date: 2024-01-20 17:46:21.425376-07:00
description: "How to: - Kuinka tehd\xE4: PHP:ss\xE4 alamerkkijonoja voi poimia `substr`-\
  \ ja `mb_substr`-funktioiden avulla. T\xE4ss\xE4 pari esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.644966-06:00'
model: gpt-4-1106-preview
summary: "PHP:ss\xE4 alamerkkijonoja voi poimia `substr`- ja `mb_substr`-funktioiden\
  \ avulla."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: - Kuinka tehdä:
PHP:ssä alamerkkijonoja voi poimia `substr`- ja `mb_substr`-funktioiden avulla. Tässä pari esimerkkiä:

```PHP
<?php
$merkkijono = "Hei, maailma!";

// Ota alamerkkijono indeksistä 0, pituus 3
$tervehdys = substr($merkkijono, 0, 3);
echo $tervehdys; // Tulostaa: Hei

// Ota loppu alkaen indeksistä 5
$loppuosa = substr($merkkijono, 5);
echo $loppuosa; // Tulostaa: maailma!

// Käytä negatiivista indeksiä viimeisten merkkien poimimiseen
$huutomerkki = substr($merkkijono, -1);
echo $huutomerkki; // Tulostaa: !
?>
```

## Deep Dive - Syväsukellus
`substr` on ollut osana PHP:tä varhaisista versioista lähtien ja auttaa nopeasti selville palan tekstistä. `mb_substr` toimii monikielisten alamerkkijonojen kanssa, tärkeä UTF-8 merkistöä käytettäessä. Vaikka `substr` on nopea ja helppo, leikattu data saattaa vaatia lisäkäsittelyä, kuten remmien poistoa tai päätellen hakua.

Monet modernit PHP kehykset, kuten Laravel, tarjoavat omia apuvälineitä merkkijonojen käsittelyyn, jotka voivat olla joustavampia mutta saattavat lisätä ylimääräisiä riippuvuuksia.

## See Also - Katso Myös
- PHP Manual substr - [php.net/manual/en/function.substr.php](https://www.php.net/manual/en/function.substr.php)
- PHP Manual mb_substr - [php.net/manual/en/function.mb-substr.php](https://www.php.net/manual/en/function.mb-substr.php)
- String handling functions in PHP - [php.net/manual/en/ref.strings.php](https://www.php.net/manual/en/ref.strings.php)
