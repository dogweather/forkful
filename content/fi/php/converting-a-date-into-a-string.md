---
title:                "PHP: Muuntaminen päivämääräksi merkkijonona"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä saattaa olla tarvetta muuttaa päivämäärät tekstiksi. Tämä voi olla tarpeellista esimerkiksi silloin, kun halutaan tulostaa päivämäärä verkkosivulle tai tallentaa se tietokantaan. PHP:lla tämä on helppoa tehdä ja tässä blogikirjoituksessa kerromme, miten se tapahtuu.

## Miten

Päivämäärien muuttaminen tekstin onnistuu PHP:lla date() -funktiolla. Tämä funktio hyödyntää käyttöjärjestelmän kellonaikaa ja oletusarvoisesti tulostaa päivämäärän muodossa "kuukausi/päivä/vuosi". Esimerkiksi:

```PHP 
<?php
$date = date('m/d/Y');
echo date;
```

Tämän tulostusten näyttäisi seuraavalta:

```
04/18/2020
```

Voit myös muuttaa päivämäärän muotoa antamalla date() -funktiolle toisen parametrin, joka määrittelee halutun muodon. Esimerkiksi:

```PHP
<?php
$date = date('d.m.y', strtotime("2020-04-18"));
echo $date;
```

Tulostus olisi tällöin:

```
18.04.20
```

## Syvemmälle

PHP:n date() -funktion avulla voit muuttaa päivämäärän lisäksi myös muita aikamuotoja, kuten esimerkiksi kellonajan tai viikonpäivän. Voit myös käyttää muita parametreja muokataksesi tulostettavaa muotoa esimerkiksi lisäämällä tekstiä tai numeroita haluttuun kohtaan.

Voit tutustua tarkemmin date() -funktioon PHP:n virallisesta dokumentaatiosta.

## Katso myös

- [PHP:n virallinen dokumentaatio date() -funktiosta](https://www.php.net/manual/en/function.date.php)
- [W3Schoolsin opas päivämäärän muuttamiseen PHP:lla](https://www.w3schools.com/php/php_date.asp)