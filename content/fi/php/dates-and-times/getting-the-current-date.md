---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- /fi/php/getting-the-current-date/
date:                  2024-02-03T19:10:27.106553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Nykyisen päivämäärän hakeminen PHP:ssa on perustehtävä, joka mahdollistaa järjestelmän päivämäärän ja ajan hakemisen sekä manipuloinnin. Tämä on elintärkeää toiminnoille, kuten lokitiedostojen kirjaaminen, viestien aikaleimaus, tapahtumien ajoittaminen tai aikaan sidottujen operaatioiden suorittaminen sovelluksissasi.

## Miten:
### Natiivi PHP
PHP:n sisäänrakennettu `date()`-funktio on suoraviivaisin tapa saada nykyinen päivämäärä. Voit muotoilla päivämäärää eri tavoin määrittelemällä muotoiluparametrin.

```php
echo date("Y-m-d"); // Tulostaa: 2023-04-01 (esimerkiksi)
echo date("l, F j, Y"); // Tulostaa: lauantai, huhtikuu 1, 2023
```

Saat päivämäärän ja ajan aikavyöhyketuella käyttämällä `DateTime`-luokkaa yhdessä `DateTimeZone`-luokan kanssa.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Tulostaa: 2023-04-01 12:00:00 (esimerkiksi)
```

### Käyttäen Carbonia (Suosittu Kolmannen Osapuolen Kirjasto)
[Carbon](https://carbon.nesbot.com/) on yksinkertainen API-laajennus `DateTime`-luokalle, joka tarjoaa selkeämmän ja sujuvamman tavan työskennellä päivämäärien ja aikojen kanssa.

Varmista ensin, että sinulla on Carbon asennettuna Composerin kautta:
```bash
composer require nesbot/carbon
```

Sen jälkeen voit käyttää sitä nykyisen päivämäärän hakemiseen:

```php
use Carbon\Carbon;

echo Carbon::now(); // Tulostaa: 2023-04-01 12:00:00 (esimerkiksi, oletusmuodossa)
echo Carbon::now()->toDateString(); // Tulostaa: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Tulostaa: lauantai, huhtikuu 1, 2023
```

Carbon rikastaa PHP:n päivämäärä- ja aikakäsittelyä lisäämällä luettavuutta ja runsaasti toiminnallisuutta ajan manipulointiin, vertailuun ja muotoiluun.
