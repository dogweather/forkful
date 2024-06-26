---
date: 2024-01-20 17:37:18.857011-07:00
description: "How to: \"Sukellus syvemm\xE4lle\" PHP:ss\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4\
  n muuntaminen merkkijonoksi onnistuu DateTime-luokalla, joka tuli k\xE4ytt\xF6\xF6\
  n PHP 5.2 -versiossa ja korvasi\u2026"
lastmod: '2024-04-05T22:51:10.819225-06:00'
model: gpt-4-1106-preview
summary: "\"Sukellus syvemm\xE4lle\" PHP:ss\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen\
  \ merkkijonoksi onnistuu DateTime-luokalla, joka tuli k\xE4ytt\xF6\xF6n PHP 5.2\
  \ -versiossa ja korvasi vanhoja toimintoja, kuten `date()`-funktion."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
"Näin tehdään:"

```PHP
<?php
$nykyinenAika = new DateTime();
echo $nykyinenAika->format('Y-m-d H:i:s'); // Tulostaa esim. "2023-04-01 12:45:31"
?>
```

## Deep Dive
"Sukellus syvemmälle"

PHP:ssä päivämäärän muuntaminen merkkijonoksi onnistuu DateTime-luokalla, joka tuli käyttöön PHP 5.2 -versiossa ja korvasi vanhoja toimintoja, kuten `date()`-funktion. Historiallisesti PHP:ssä käytettiin `date()`-funktiota ja `strtotime()`-funktiota päivämäärän muotoiluun ja käsittelyyn. DateTime tarjosi paremman objektilähtöisen lähestymistavan.

Vaihtoehtona `format()`-metodille, voit käyttää `date_format()`-funktiota, joka ottaa vastaan DateTime-olioita:

```PHP
<?php
$nykyinenAika = new DateTime();
echo date_format($nykyinenAika, 'Y-m-d H:i:s'); // Sama tulostus
?>
```

Merkkijonon muotoilussa `Y-m-d H:i:s` on yleinen muotoilusyntaksi. Tässä `Y` on vuosi nelinumeroisena, `m` on kuukausi, `d` on päivä, `H` on tunti 24h-muodossa, `i` on minuutit ja `s` ovat sekunnit. PHP tarjoaa monia muitakin muotoilumerkkejä, joiden avulla voi esittää päivämääriä todella monipuolisesti.

## See Also
"Katso myös"

- PHP:n virallinen dokumentaatio DateTime-luokasta: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- PHP:n päivämäärä- ja aikaformaatit: [php.net/manual/en/datetime.format.php](https://www.php.net/manual/en/datetime.format.php)
- PHP:n date()-funktion dokumentaatio: [php.net/manual/en/function.date.php](https://www.php.net/manual/en/function.date.php)
