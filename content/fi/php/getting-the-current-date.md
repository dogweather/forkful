---
title:    "PHP: Nykyisen päivämäärän saaminen."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monissa koodauksen projekteissa tarvitaan esimerkiksi ajankohtaista päivämäärää kirjauksia ja tapahtumia varten. PHP:ssa on valmiit toiminnot, joiden avulla tämä onnistuu helposti ja tarkasti.

## Miten

### Perusversio

Paras tapa saada nykyinen päivämäärä PHP:ssa on käyttää `date()`-funktiota. Se palauttaa merkkijonon, jossa on nykyinen päivämäärä ja aika halutussa muodossa.

```PHP
$date = date("d.m.Y H:i:s");
echo $date;
```

Tulostaa esimerkiksi `27.08.2020 10:00:00`.

### Eri paikkakuntien aikavyöhykkeet

`date_default_timezone_set()`-funktiolla voidaan asettaa haluttu aikavyöhyke. Jos tämä jätetään pois, käytetään palvelimen aikavyöhykettä.

```PHP
date_default_timezone_set("Europe/Helsinki");

$date = date("d.m.Y H:i:s");
echo $date;
```

Tulostaa esimerkiksi `27.08.2020 13:00:00`, jos palvelin sijaitsee Helsingissä.

### Unix-aikaleima

PHP:n `time()`-funktio palauttaa Unix-aikaleiman, joka on sekuntien määrä kulunut vuodesta 1970. Tätä voi käyttää `date()`-funktion kanssa hankkimaan halutun päivämäärän.

```PHP
$time = time(); // esim. 1598545200
$date = date("d.m.Y H:i:s", $time);
echo $date;
```

Tulostaa esimerkiksi `27.08.2020 10:00:00`.

## Syvenny

PHP:n `date()`-funktio käyttää palvelimen asetettua aikavyöhykettä. Jos käytössäsi on esimerkiksi verkkosivusto, jota käyttävät käyttäjät ympäri maailman, olisi hyvä antaa käyttäjille mahdollisuus asettaa haluamansa aikavyöhyke. Tämä onnistuu esimerkiksi käyttämällä `DateTime`-luokkaa ja antamalla käyttäjän valita aikavyöhyke pudotusvalikosta.

Esimerkiksi jos jokin tapahtuma järjestetään Chicago:ssa, mutta käyttäjä on Helsingissä, hän näkee tapahtuman ajankohdan oikeassa ajassa käyttäjän valitsemassa aikavyöhykkeessä.

```PHP
// Palvelimen aikavyöhyke
$date = new DateTime();
$date = $date->format("d.m.Y H:i:s");
echo $date; // Palauttaa esim. 02.09.2020 08:00:00

// Käyttäjän valitsema aikavyöhyke
$user_timezone = "Europe/Helsinki";
$date = new DateTime();
$date->setTimezone(new DateTimeZone($user_timezone));
$date = $date->format("d.m.Y H:i:s");
echo $date; // Palauttaa esim. 02.09.2020 13:00:00
```

## Katso myös

- PHP:n virallinen dokumentaatio: https://www.php.net/manual/en/function.date.php
- Aikavyöhykkeiden lista: https://www.php.net/manual/en/timezones.php