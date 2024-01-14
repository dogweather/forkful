---
title:                "PHP: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit tietää nykyisen päivämäärän? On monia tapoja, joilla tämä tieto voi olla hyödyllinen ohjelmoinnissa. Se voi auttaa sinua seuraamaan aikaa ja päivämääriä, tallentamaan käyttäjien tekemiä muutoksia ja luomaan ajastettuja tehtäviä.

## Miten
```PHP
<?php
$date = date("d.m.Y"); //tallentaa päivämäärän muuttujaan
echo $date; //tulostaa nykyisen päivämäärän muodossa pp.kk.vvvv
```

Tämän yksinkertaisen koodinpätkän avulla voit saada nykyisen päivämäärän PHP:ssä. Voit myös muuttaa date()-funktion parametreja saadaksesi haluamasi päivämäärän muodossa. Esimerkiksi voit käyttää "l" saadaksesi päivän nimen tai "F" saadaksesi kuukauden nimen.

Voit myös käyttää time()-funktiota saadaksesi nykyisen ajan. Tämä palauttaa UNIX-timestampin, joka on sekuntien määrä kulunut 1. tammikuuta 1970 klo 00:00:00 GMT:stä lähtien.

```PHP
<?php
$time = time(); //palauttaa UNIX-timestampin
echo $time; //tulostaa esimerkiksi 1621713943
```

Voit sitten käyttää date()-funktiota muuntaaksesi UNIX-timestampin päivämääräksi:

```PHP
<?php
$time = time();
$date = date("d.m.Y", $time); //muuntaa UNIX-timestampin päivämääräksi
echo $date; //tulostaa nykyisen päivämäärän muodossa pp.kk.vvvv
```

## Syventävä sukellus
PHP:n date()-funktio käyttää paikallista aikavyöhykettä näyttääkseen päivämäärän ja ajan. Voit asettaa aikavyöhykkeen haluamaksesi käyttämällä date_default_timezone_set() -funktiota. Voit löytää listan tuetuista aikavyöhykkeistä PHP:n dokumentaatiosta.

Voit myös manipuloida päivämäärää ja aikaa käyttämällä strtotime()-funktiota. Se ottaa päivämäärän ja ajan merkkijonona ja muuntaa sen UNIX-timestampiksi.

```PHP
<?php
$date = strtotime("next Monday"); //muuntaa seuraavan maanantain UNIX-timestampiksi
```

Voit sitten käyttää tätä timestampia date()-funktiossa saadaksesi haluamasi päivämäärän. Tämä voi olla hyödyllistä esimerkiksi luotaessa ajastettuja tapahtumia.

## Katso myös
- [PHP:n date()-funktio](https://www.php.net/manual/en/function.date.php)
- [PHP:n aikavyöhykkeet](https://www.php.net/manual/en/timezones.php)
- [PHP:n strtotime()-funktio](https://www.php.net/manual/en/function.strtotime.php)