---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän muuttaminen merkkijonoksi on prosessi, jossa päivämäärä-objekti muunnetaan merkkijonoksi. Ohjelmoijat tekevät tämän tietyissä tilanteissa, kuten kun halutut muodot tai formaatit ovat välttämättömiä tiedonsiirtoa tai tietojen esittämistä varten.

## Miten se tehdään:

PHP:ssä päivämäärän muuntamiseksi merkkijonoksi käytetään `date_format`-funktiota. Tämä funktio ottaa kaksi argumenttia: päivämäärä-objektin ja halutun muotoisen merkkijonoformaatin.

```PHP
<?php
$date = new DateTime('2020-01-01');
echo date_format($date, 'Y-m-d');  // Tulostaa: 2020-01-01
?>
```

## Syvä Sukellus:

Päivämäärän esittämisen merkkijonona on pitkä historia. Sen alkuperä ulottuu ohjelmoinnin alkuhämäriin, jolloin tietojen tallennus- ja esitysformaatti oli rajallinen.

Vaihtoehtona päivämäärä voidaan muuntaa myös strtotime-funktion avulla, joka muuntaa minkä tahansa tekstisträngin päivämääräksi ja ajaksi. 

```PHP
<?php
$date = strtotime("2020-01-01");
echo date('Y-m-d', $date);  // Tulostaa: 2020-01-01
?>
```

Suorituskyvyn näkökulmasta funktion `date_format` käyttö on parempi kuin `strtotime`, koska DateTime-objekti on jo olemassa ja se voidaan muuntaa suoraan merkkijonoksi.

## Katso myös:

1. PHP: DateTime - [Manual](https://www.php.net/manual/en/class.datetime.php)
2. PHP: date_format - [Manual](https://www.php.net/manual/en/datetime.format.php)
3. PHP: strtotime - [Manual](https://www.php.net/manual/en/function.strtotime.php)