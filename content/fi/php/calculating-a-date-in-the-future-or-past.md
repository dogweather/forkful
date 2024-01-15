---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "PHP: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi?

Tulevaisuuden tai menneisyyden päivämäärän laskemisella voi olla monia käyttötarkoituksia, kuten aikataulujen tai tapahtumien suunnittelu, laskujen eräpäivien määrääminen tai tietyn hetken laskeminen tietyn tapahtuman jälkeen. PHP tarjoaa monia käteviä toimintoja, jotka helpottavat päivämäärien laskemista ja manipulointia.

## Miten tehdä?

```PHP
// Tulevaisuuden päivän laskeminen
$päivä = date('d'); //Hae nykyinen päivä
$kuukausi = date('m'); //Hae nykyinen kuukausi
$vuosi = date('Y'); //Hae nykyinen vuosi

$näytä_viikon_päivä = date('l', strtotime('+7 days')); // Lasketaan päivän olemassaolosta 7 päivää
$uusi_päivä = date('d-m-Y', strtotime('+1 day', strtotime('05-04-2022'))); // Lisätään yksi päivä tiettyyn päivämäärään

// Menneisyyden päivän laskeminen
$vanha_päivä = date('Y-m-d', strtotime('-1 day', strtotime('05-04-2022'))); // Vähennetään yksi päivä tietystä päivämäärästä
$ero_päivät =abs(strtotime('2022-05-05') - strtotime('2022-04-05')); // Lasketaan päivien määrä kahden päivämäärän välillä
```

Tässä esimerkissä käytetään PHP:n `date()` ja `strtotime()` -funktioita, jotka ovat hyödyllisiä päivämäärien manipuloinnissa. `date()`-funktio palauttaa halutun päivämäärän muodossa, joka on määritelty parametrien avulla. `strtotime()`-funktio sallii päivämäärälausekkeiden muokkaamisen ja tietyn ajan lisäämisen tai vähentämisen päivämäärästä. Näiden toimintojen yhdistelmänä voit helposti laskea tulevaisuuden tai menneisyyden päivämääriä halutuilla muutoksilla.

## Syvällisempi sukellus

PHP tarjoaa myös muita hyödyllisiä toimintoja päivämäärien laskemiseen ja manipulointiin, kuten `date_modify()` ja `date_interval_create_from_date_string()`.

`date_modify()`-toiminnon avulla voit tehdä monimutkaisempia muutoksia päivämäärään, kuten lisätä tai vähentää useita päiviä tai kuukausia. Voit myös käyttää `date_interval_create_from_date_string()`-toimintoa luomaan päivämäärälausekkeita ja lisäämään ne haluttuun päivämäärään.

PHP:n `DateTime`-luokka tarjoaa myös paljon hyödyllisiä menetelmiä päivämäärien käsittelyyn, kuten `add()` ja `sub()`-menetelmät, jotka toimivat samalla tavalla kuin `strtotime()`-funktio. Lisäksi voit käyttää `format()`-menetelmää määrittääksesi päivämäärän muodon ja näyttääksesi sen halutussa muodossa.

## Katso myös

- PHP viralliset dokumentaatiot: https://www.php.net/manual/en/datetime.format.php
- FreeCodeCampin opas päivämäärien laskemiseen PHP:llä: https://www.freecodecamp.org/news/how-to-work-with-dates-and-times-in-php/