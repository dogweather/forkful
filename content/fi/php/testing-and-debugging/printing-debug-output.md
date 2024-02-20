---
date: 2024-01-20 17:53:07.984803-07:00
description: "PHP-koodauksessa debug-tulostus auttaa l\xF6yt\xE4m\xE4\xE4n ja ymm\xE4\
  rt\xE4m\xE4\xE4n ongelmia. Koodarit k\xE4ytt\xE4v\xE4t sit\xE4, koska n\xE4kee suoraan,\
  \ mit\xE4 koodi tekee (tai ei tee)."
lastmod: 2024-02-19 22:05:15.552923
model: gpt-4-1106-preview
summary: "PHP-koodauksessa debug-tulostus auttaa l\xF6yt\xE4m\xE4\xE4n ja ymm\xE4\
  rt\xE4m\xE4\xE4n ongelmia. Koodarit k\xE4ytt\xE4v\xE4t sit\xE4, koska n\xE4kee suoraan,\
  \ mit\xE4 koodi tekee (tai ei tee)."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?

PHP-koodauksessa debug-tulostus auttaa löytämään ja ymmärtämään ongelmia. Koodarit käyttävät sitä, koska näkee suoraan, mitä koodi tekee (tai ei tee).

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
