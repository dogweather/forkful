---
title:    "PHP: Väliaikaisen tiedoston luominen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi
Luomalla väliaikaisia tiedostoja voit helposti tallentaa ja käyttää väliaikaista tietoa, kuten väliaikaisia sähköpostiviestejä tai laskurin tuloksia, koodissasi.

## Miten
Väliaikaisen tiedoston luominen PHP:lla on helppoa. Sinun tarvitsee vain käyttää "tempnam()" -funktiota ja antaa sille kaksi argumenttia: hakemisto, johon tiedosto tallennetaan, ja tiedoston alkuosa. Alla on esimerkki koodista ja siihen liittyvä ulostulo:

```PHP
<?php
$tempfile = tempnam("/tmp", "testfile");
echo $tempfile;
```
**Ulostulo:** /tmp/testfile12345

## Syvällisempi tutkimus
PHP:n "tempnam()" -funktio luo väliaikaisen tiedoston annettuun hakemistoon ja palauttaa sen sijainnin. Jos sinulla on jo hakemisto, johon haluat tallentaa tiedoston, voit käyttää "tmpfile()" -funktiota sen sijaan. Nämä funktiot myös takaa, että tiedoston nimi on uniikki kaikille luoduille väliaikaisille tiedostoille.

## Katso myös
- [PHP:n virallinen dokumentaatio "tempnam()" -funktiosta](https://www.php.net/manual/en/function.tempnam.php)
- [PHP:n virallinen dokumentaatio "tmpfile()" -funktiosta](https://www.php.net/manual/en/function.tmpfile.php)