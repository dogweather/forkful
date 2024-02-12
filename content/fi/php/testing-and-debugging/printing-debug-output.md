---
title:                "Virheenjäljitystulosteiden tulostaminen"
aliases:
- /fi/php/printing-debug-output.md
date:                  2024-01-20T17:53:07.984803-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
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
