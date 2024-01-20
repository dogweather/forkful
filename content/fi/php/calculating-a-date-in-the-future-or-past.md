---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "PHP: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lasketaan tulevaisuuden tai menneisyyden päivämäärä voidaan määrittää ajan kulumisen ohjelmointimaailmassa. Ohjelmoijat tekevät tämän, kun he tarvitsevat määrittelemään säännöllisen toimen tai tapahtuman suorituspäivän, esim. laskut, muistutukset tai päivitykset.

## Näin se tehdään:

PHP:n sisäänrakennettua DateTime-luokkaa voidaan käyttää helposti päivämäärien laskemiseen. Tässä on kaksi perusesimerkkiä, kuinka laskea tulevaisuuden tai menneisyyden päivämäärä:

```PHP
<?php
// Lasketaan 10 päivää tulevaisuudessa
$date = new DateTime(null, new DateTimeZone('Europe/Helsinki'));
$date->add(new DateInterval('P10D'));
echo $date->format('Y-m-d');

// Lasketaan 30 päivää menneisyydessä
$date = new DateTime(null, new DateTimeZone('Europe/Helsinki'));
$date->sub(new DateInterval('P30D'));
echo $date->format('Y-m-d');
?>
```

## Syvempi sukellus:

PHP:n DateTime-luokka on osa PHP:n core API:a ja se on ollut saatavilla PHP 5.2:sta lähtien. Sitä pidetään parempana vaihtoehtona kuin vanhempi `date`-funktio syistä, kuten sen kyvystä käsitellä aikavyöhykkeitä ja sen objektisuuntautunut luonne, joka sopii paremmin moderniin PHP-koodiin.

On kuitenkin myös muita tapoja, kuten käyttää PHP:n `strtotime`-funktiota:

```PHP
<?php
// 15 päivää tulevaisuudessa
$date = date('Y-m-d', strtotime('+15 days'));
echo $date;

// 50 päivää menneisyydessä
$date = date('Y-m-d', strtotime('-50 days'));
echo $date;
?>
```

Koska `strtotime` muuttaa merkkijonon unix aikaleimaksi, se ei pysty käsittelemään kaikkia DateTime-luokan tarjoamia ominaisuuksia, kuten aikavyöhykkeitä.

## Katso myös:

- PHP:n virallinen dokumentaatio DateTime-luokasta: [php.net](https://www.php.net/manual/en/class.datetime.php)
- PHP:n virallinen dokumentaatio DateInterval-luokasta: [php.net](https://www.php.net/manual/en/class.dateinterval.php)
- Hyvä tutorial päivämäärän manipuloinnista PHP:ssä: [tutorialspoint.com](https://www.tutorialspoint.com/php/php_date_time_functions.htm)