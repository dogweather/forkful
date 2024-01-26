---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:37.726868-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Arpajaiset ilman numeroita? Ei onnistu. Koodarit tuottavat satunnaislukuja testeihin, turvallisuuteen ja kun tarvitaan ennakoimatonta tulosdataa. Hyvä satunnaisuus tarkoittaa uskottavaa simulointia ja parempaa tietoturvaa.

## How to: - Näin teet sen:
PHP:ssä satunnaislukuja saa heittämällä kolikolla: `rand` tai `mt_rand` funktiota käyttämällä. PHP 7.0 lähtien suositellaan `random_int`, koska se on turvallisempi.

```PHP
<?php
// Perus satunnaisluku väliltä 1-10
echo rand(1, 10);

// mt_rand on nopeampi, mutta ei niin turvallinen
echo mt_rand(1, 10);

// PHP 7.0+ turvallisempi random_int
echo random_int(1, 10);
?>
```
Kokeile ja näe numerot pompivan.

## Deep Dive - Syväsukellus:
Aikoinaan, `rand()` ja `mt_rand()` olivat go-to. `mt_rand()` on nopeampi mutta ei niin turvallinen. `random_int()` käyttää vahvaa kryptograafista pseudosatunnaislukugeneraattoria, ja se on lyömättömillä turvatiedoilla varustettu. Ei enää vain numeerista arpomista; kyse on ennustamattomuuden vakavasta businessista.

## See Also - Katso myös:
- [PHP Manual on `rand`](https://www.php.net/manual/en/function.rand.php)
- [PHP Manual on `mt_rand`](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP Manual on `random_int`](https://www.php.net/manual/en/function.random-int.php)
