---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:00.581148-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta PHP:ss\xE4\
  \ tarkoittaa tekstin, joka edustaa p\xE4iv\xE4m\xE4\xE4r\xE4\xE4 ja/tai aikaa, muuntamista\
  \ PHP:n `DateTime`-objektiksi tai\u2026"
lastmod: '2024-03-13T22:44:56.665538-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta PHP:ss\xE4 tarkoittaa\
  \ tekstin, joka edustaa p\xE4iv\xE4m\xE4\xE4r\xE4\xE4 ja/tai aikaa, muuntamista\
  \ PHP:n `DateTime`-objektiksi tai muihin p\xE4iv\xE4ys-/aikamuotoihin."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Kuinka:
PHP:n sisäänrakennettu `DateTime`-luokka tarjoaa tehokkaan joukon funktioita päivämäärien jäsentämiseksi ja käsittelyksi. Voit luoda `DateTime`-instanssin päivämäärämerkkijonosta käyttämällä konstruktoria ja sitten muotoilla sen tarpeen mukaan. Näin se tehdään:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Tulostus: 2023-04-25 15:30:00
```

Käsitelläksesi merkkijonoja, jotka noudattavat epästandardimuotoja, voit käyttää `createFromFormat`-metodia, joka mahdollistaa syöttöpäivämäärän tarkan muodon määrittämisen:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Tulostus: 2023-04-25 15:30:00
```

Monimutkaisempaan jäsentämiseen, jota `DateTime` ei ehkä suoraan tue, PHP tarjoaa `strtotime`-funktion, joka yrittää jäsentää minkä tahansa englanninkielisen tekstuaalisen päivämäärä- ja aikakuvauksen Unix-aikaleimaksi:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// Tulostus vaihtelee nykyisen päivämäärän mukaan, esim. "2023-05-04"
```

**Kolmannen osapuolen kirjastojen käyttö:**

Vaikka PHP:n sisäänrakennetut funktiot kattavat laajan valikoiman käyttötarkoituksia, saatat joskus tarvita monimutkaisempia jäsentämiskykyjä. Carbon-kirjasto, joka on PHP:n DateTime-luokan laajennus, tarjoaa rikkaan joukon ominaisuuksia päivämäärän/ajan käsittelyyn:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// Tulostus vaihtelee, esim. "2023-04-26 00:00:00"
```

Carbonin `parse`-metodi osaa älykkäästi käsitellä monia päivämäärä- ja aikamuotoja, mikä tekee siitä korvaamattoman työkalun sovelluksille, jotka vaativat joustavaa päivämäärän jäsentämistoiminnallisuutta.
