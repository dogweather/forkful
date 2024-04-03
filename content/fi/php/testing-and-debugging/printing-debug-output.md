---
date: 2024-01-20 17:53:07.984803-07:00
description: "How to: - Kuinka: Debug-tulostusta voi tehd\xE4 `echo` ja `print_r`\
  \ avulla. T\xE4ss\xE4 pari esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.658469-06:00'
model: gpt-4-1106-preview
summary: "Debug-tulostusta voi tehd\xE4 `echo` ja `print_r` avulla."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to: - Kuinka:
Debug-tulostusta voi tehdä `echo` ja `print_r` avulla. Tässä pari esimerkkiä:

```php
<?php
// Yksinkertainen echo-komento
$muuttuja = "Hei maailma!";
echo $muuttuja; // Tulostaa: Hei maailma!

// Array ja print_r
$array = array('yksi', 'kaksi', 'kolme');
print_r($array);
/* Tulostaa:
Array
(
    [0] => yksi
    [1] => kaksi
    [2] => kolme
)
*/
```

## Deep Dive - Syväsukellus:
Historiallisesti PHP on tarjonnut useita built-in funktioita debug-tulostusta varten. `print_r`, `var_dump` ja `var_export` ovat tyypillisiä työkaluja. Ne eroavat tiedon esittämisessä: `var_dump` näyttää tyypit ja pituudet, `var_export` taas palauttaa validin PHP-koodin.

PHP:n error_log-funktio mahdollistaa virhetietojen kirjoittamisen lokiin, mikä on välttämätöntä tuotantoympäristössä. Xdebug-laajennus tuo kehittyneitä debuggausominaisuuksia, kuten stack trace.

Viimeisimpänä huomiona, debug-tulostusta ei pitäisi jättää tuotantokoodiin suorituskyvyn ja tietoturvan vuoksi.

## See Also - Katso Myös:
- PHP:n virallinen dokumentaatio `print_r`: https://www.php.net/manual/en/function.print-r.php
- PHP Documentation on `var_dump`: https://www.php.net/manual/en/function.var-dump.php
- Xdebug, PHP debugger: https://xdebug.org/
- Error logging in PHP: https://www.php.net/manual/en/function.error-log.php
