---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "PHP: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa on kätevä muuttaa päivämäärä merkkijonoksi, esimerkiksi näyttääkseen sen ihmisille luettavassa muodossa tai tallentaakseen sen tietokantaan. PHP:ssä tämän tekeminen on helppoa ja nopeaa.

## Miten

Muuttaaksesi päivämäärän merkkijonoksi PHP:ssä, voit käyttää `date()` funktiota ja antaa sille sekä alkuperäisen päivämäärän että muotoilun, jota haluat käyttää. Esimerkiksi:

```PHP
$date = '2019-11-11'; // alkuperäinen päivämäärä
$muoto = 'j.n.Y'; // muotoilu, jossa j = päivä, n = kuukausi ja Y = vuosi
$muunnettu_pvm = date($muoto, strtotime($date)); // muunnetaan päivämäärä
echo $muunnettu_pvm; // tulostaa "11.11.2019"
```

Huomaa, että `strtotime()` funktiolla muutetaan päivämäärä ensin numeroarvoksi ja vasta sitten `date()` funktio muuntaa sen merkkijonoksi.

Voit myös käyttää `DateTime` luokkaa, joka tarjoaa enemmän vaihtoehtoja päivämäärän muotoiluun:

```PHP
$date = '2019-11-11'; // alkuperäinen päivämäärä
$dtime = new DateTime($date); // luodaan uusi DateTime objekti päivämäärällä
echo $dtime->format('Y-m-d'); // tulostaa "2019-11-11"
echo $dtime->format('d.m.Y'); // tulostaa "11.11.2019"
echo $dtime->format('l, jS F Y'); // tulostaa "Monday, 11th November 2019"
```

## Syvällinen sukellus

Päivämäärän muuttaminen merkkijonoksi voi kuulostaa yksinkertaiselta, mutta voi aiheuttaa ongelmia, jos et huomioi muotoilua oikein. Esimerkiksi jos käytät merkintöjä `m` ja `d` kuukausien ja päivien sijaan, saatat saada odottamattomia tuloksia kuten "01.01.2020" sijaan "1.1.2020". Lisäksi, jos käytät `Y` merkintää vuodelle nelinumeroisen sijaan, muuntuu vuosi automaattisesti kahteen numeroon vuosisadan mukaan.

On myös tärkeää huomata, että `date()` ja `DateTime` funktioiden avulla muutettu päivämäärä on edelleen merkkijono, eikä sitä voi käyttää laskutoimituksissa tai vertailuissa päivämäärämuotoisten muuttujien kanssa.

## Katso myös

- PHP date() funktio: https://www.php.net/manual/en/function.date.php
- PHP DateTime luokka: https://www.php.net/manual/en/class.datetime.php