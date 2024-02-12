---
title:                "Merkkijonon interpolointi"
aliases:
- /fi/php/interpolating-a-string/
date:                  2024-01-20T17:51:36.860617-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Interpolointi tarkoittaa muuttujien arvojen yhdistämistä merkkijonoon. Koodarit käyttävät sitä dynaamisten viestien luomiseen ja koodin selkeyden parantamiseen.

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
