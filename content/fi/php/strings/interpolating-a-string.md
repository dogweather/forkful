---
date: 2024-01-20 17:51:36.860617-07:00
description: "How to: - Miten: Interpoloinnin juuret ovat varhaisissa ohjelmointikieliss\xE4\
  , kuten Perliss\xE4, joka PHP:n syntaksia on voimakkaasti vaikuttanut.\u2026"
lastmod: '2024-04-05T21:53:58.214208-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkijonon interpolointi
weight: 8
---

## How to: - Miten:
```php
<?php
$juoma = "kahvi";
$aika = "aamulla";

// Kaksoislainausmerkkien sisällä
$viesti = "Miten olisi kuppi $juoma$aika?";
echo $viesti; // Miten olisi kuppi kahviaamulla?

// Käyttäen aaltosulkeita selvyyden vuoksi
$viesti = "Miten olisi kuppi {$juoma} {$aika}?";
echo $viesti; // Miten olisi kuppi kahvi aamulla?

// Huom. Yksinkertaiset lainausmerkit eivät interpoloi
$viesti = 'Miten olisi kuppi $juoma $aika?';
echo $viesti; // Miten olisi kuppi $juoma $aika?
?>
```

## Deep Dive - Syväsukellus:
Interpoloinnin juuret ovat varhaisissa ohjelmointikielissä, kuten Perlissä, joka PHP:n syntaksia on voimakkaasti vaikuttanut. Alternatiiveina ovat yhdistämisoperaattori `.` tai `sprintf()`-funktio, jotka voivat lisätä koodin monimutkaisuutta:

```php
$viesti = 'Miten olisi kuppi ' . $juoma . ' ' . $aika . '?';
```

Interpoloinnin toteutus PHP:ssä on tehokas sillä merkkijonoon voi liittää muuttujia suoraan, kun käyttää kaksoislainausmerkkejä tai heredoc-syntaksia. Muista käyttää aaltosulkeja (`{}`) sekavuuden välttämiseksi, erityisesti jos muuttujan nimi ympäröidään muulla tekstillä.

## See Also - Katso Myös:
- PHP manuaali merkkijonoista: https://www.php.net/manual/en/language.types.string.php
- PHP `sprintf`-funktion dokumentaatio: https://www.php.net/manual/en/function.sprintf.php
