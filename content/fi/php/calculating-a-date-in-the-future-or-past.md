---
title:                "PHP: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Jokainen PHP-ohjelmoija tietää, että päivämäärien laskeminen voi olla hankalaa ja aikaa vievää työtä. Kuitenkin pystyäksesi luomaan dynaamisia verkkosovelluksia, tarvitset kyvyn laskea päivämääriä tulevaisuudessa tai menneisyydessä. Tässä blogikirjoituksessa opit, kuinka voit toteuttaa tämän PHP:llä helposti ja tehokkaasti.

## Kuinka

PHP:ssä on valmiina joitakin hyödyllisiä toimintoja päivämäärien laskemiseen tulevaisuudessa tai menneisyydessä. Yksi näistä on "strtotime" -funktio, joka muuntaa annetun päivämäärän aikaleimaksi. Voit käyttää tätä aikaleimaa sitten esimerkiksi date-funktion kanssa määrittääksesi tietyn päivämäärän tulevaisuudessa tai menneisyydessä.

```
<?php

// Lasketaan päivämäärä yhden kuukauden päästä
$tulevaisuuden_paivamaara = strtotime("+1 month");

echo date("d.m.Y", $tulevaisuuden_paivamaara); // Tulostaa esim. "20.05.2021"

// Lasketaan päivämäärä kuusi kuukautta taaksepäin
$mennytyysipaivamaara = strtotime("-6 months");

echo date("d.m.Y", $mennytyysipaivamaara); // Tulostaa esim. "20.11.2020"

?>
```

Voit myös asettaa halutun päivämäärän sopivassa muodossa "strtotime" -funktion ensimmäisenä parametrina. Esimerkiksi "25 December 2021" tai "next Monday".

## Syvällisempi sukellus

"strtotime" -funktio tukee monia erilaisia päivämääränmuotoja ja lisäksi voit käyttää siihen myös useita avainsanoja, kuten "+1 year" tai "next week". "Date" -funktio taas mahdollistaa päivämäärän muotoilun halutun mallin perusteella.

Voit myös käyttää muita PHP:n sisäänrakennettuja funktioita päivämäärien laskemiseen, kuten "mktime" ja "cal_days_in_month".

## Katso myös

- PHP:n virallinen dokumentaatio päivämäärien laskemisesta: https://www.php.net/manual/en/function.strtotime.php
- Kätevät esimerkit ja lisätietoa päivämäärien laskemisesta PHP:ssä: https://www.w3schools.com/php/func_date_strtotime.asp
- PHP-päivämääräfunktioiden vertailu: https://www.toptal.com/software/definitive-php-datetime-cheatsheet