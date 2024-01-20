---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstifilin lukeminen tarkoittaa sen sisällön saamista merkkijonona tai datapalasena. Ohjelmoijat tekevät tämän tiedon tallentamiseksi, käsitelläkseen sitä tai kommunikoidakseen toisten ohjelmien kanssa.

## Kuinka:

PHP:n sisäänrakennettu funktio `file_get_contents()` tekee tekstifilin lukemisen helpoksi. Katso esimerkki alla:

```PHP
<?php
$file = 'example.txt';
$content = file_get_contents($file);
echo $content;
?>
```
Oletetaan, että `example.txt` sisältää tekstin "Hello, World!". Yllä oleva skripti tulostaa:
```
Hello, World!
```

## Syvä Sukellus:

Tekstifilin lukeminen on perusosa ohjelmoinnin historiaa, ja se on ollut mukana ohjelmoinnissa alusta alkaen. PHP:ssä on useita tapoja lukea tiedostoja, esim. `fopen()`, `fread()`, mutta `file_get_contents()` on helpoin ja kompaktin koodauksen vuoksi usein suositeltu.

Valittu menetelmä riippuu usein kyseisestä sovelluksesta ja sen erityistarpeista. Esimerkiksi, jos tiedoston koko on erittäin suuri, 'fread()' saattaa olla parempi vaihtoehto, koska se lukee tiedoston pienissä palasissa, vähentäen muistin käyttöä.

Kuten useimmissa ohjelmointikielissä, PHP:llä lukiessa tiedosto on avattava ennen kuin sitä voidaan lukea ja suljettava sen jälkeen.

## Katso Myös:

1. PHP Official Documentation: file_get_contents() - [https://www.php.net/manual/en/function.file-get-contents.php](https://www.php.net/manual/en/function.file-get-contents.php)

2. PHP Official Documentation: fopen() - [https://www.php.net/manual/en/function.fopen.php](https://www.php.net/manual/en/function.fopen.php)

3. PHP Official Documentation: fread() - [https://www.php.net/manual/en/function.fread.php](https://www.php.net/manual/en/function.fread.php)