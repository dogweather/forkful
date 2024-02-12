---
title:                "Tulevan tai menneen päivämäärän laskeminen"
aliases:
- /fi/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:28.404885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja miksi?
Laskemme päivämääriä tulevaisuudessa tai menneisyydessä hallitaksemme aikatauluja ja määrittäksemme määräaikoja. Ohjelmoijat tekevät sen automatisoidakseen muistutuksia, vanhenemispäiviä ja tapahtumien aikatauluja.

## How to - Kuinka tehdä:
Php:ssä käytämme `DateTime`-luokkaa ja `DateInterval`-luokkaa päivämäärien laskemiseen.

```php
<?php
// Päivämäärä tänään, ja lasketaan 10 päivää eteenpäin
$date = new DateTime();
$date->add(new DateInterval('P10D'));
echo $date->format('Y-m-d') . "\n"; // Esim. tulostus: 2023-04-10

// Päivämäärä tänään, ja mennään 5 päivää taaksepäin
$date->sub(new DateInterval('P5D'));
echo $date->format('Y-m-d') . "\n"; // Esim. tulostus: 2023-03-26
?>
```

## Deep Dive - Syväsukellus:
Ennen `DateTime`-luokkaa, PHP-kehittäjät käyttivät `strtotime`-funktiota, joka on edelleen käytössä. `DateTime` tarjoaa kuitenkin enemmän joustavuutta ja on objektilähtöisempi lähestymistapa. Vaihtoehtoisesti päivämäärien käsittelyyn voi käyttää kirjastoja kuten Carbon tai Moment.php, jotka tarjoavat lisätoiminnallisuuksia ja selkeämmän syntaksin. Päivämäärien laskemisessa on otettava huomioon aikavyöhykkeet, karkausvuodet ja kesäaika.

## See Also - Katso myös:
- PHP.net `DateTime`: https://www.php.net/manual/en/class.datetime.php
- PHP.net `DateInterval`: https://www.php.net/manual/en/class.dateinterval.php
- Carbon date library for PHP: https://carbon.nesbot.com/
- Moment.php date library inspired by Moment.js: https://momentphp.com/
