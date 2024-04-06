---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.106553-07:00
description: "Miten: PHP:n sis\xE4\xE4nrakennettu `date()`-funktio on suoraviivaisin\
  \ tapa saada nykyinen p\xE4iv\xE4m\xE4\xE4r\xE4. Voit muotoilla p\xE4iv\xE4m\xE4\
  \xE4r\xE4\xE4 eri tavoin m\xE4\xE4rittelem\xE4ll\xE4\u2026"
lastmod: '2024-03-13T22:44:56.666685-06:00'
model: gpt-4-0125-preview
summary: "PHP:n sis\xE4\xE4nrakennettu `date()`-funktio on suoraviivaisin tapa saada\
  \ nykyinen p\xE4iv\xE4m\xE4\xE4r\xE4."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

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
