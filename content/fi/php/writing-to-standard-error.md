---
title:                "Kirjoittaminen standardi virheeseen"
html_title:           "PHP: Kirjoittaminen standardi virheeseen"
simple_title:         "Kirjoittaminen standardi virheeseen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen virheilmoituksille (standard error) on olennainen osa ohjelmointia, sillä se auttaa löytämään ja korjaamaan virheitä koodissa. Se myös helpottaa ohjelmoijaa ymmärtämään, mitä koodissa tapahtuu ja miksi se ei toimi odotetusti.

## Kuinka kirjoittaa virheilmoituksille (standard error)

Kirjoittamalla virheitä standard erroriin voidaan käyttää PHP:n `fwrite`-funktiota. Se ottaa kaksi parametria; ensimmäinen on tiedostodeskriptori ja toinen on merkkijono, joka tulostetaan standard erroriin. Tiedostodeskriptori voidaan saada käyttämällä `fopen`-funktiota ja parametrina annetaan `php://stderr`.

```
<?php
$fd = fopen('php://stderr', 'w');
fwrite($fd, 'Virhe: Tämä on virheellinen merkkijono.' . PHP_EOL);
fclose($fd);
```

**Tulos:**
>Virhe: Tämä on virheellinen merkkijono.

## Syventävä tieto

Standard error on yksi kolmesta tavanomaisesta tiedostovirtareitistä PHP:ssä. Se on tarkoitettu virheilmoitusten ja varoitusviestien tulostamiseen. Virheitä ja varoituksia voidaan myös ohjata toiseen tiedostoon tai jopa järjestelmän lokitiedostoon, mikäli halutaan.

Lisäksi virheilmoituksia voidaan tallentaa myös muuttujaan käyttämällä `stderr`-arvoa `ini_set`-funktiolla. Tämän avulla voidaan myös helposti muokata, mihin virheet ja varoitukset ohjataan.

```
<?php
ini_set('error_log', 'virheet.txt');
ini_set('log_errors', 1);

// ...koodi, joka voi aiheuttaa virheitä...

error_log('Virhe tiedoston avaamisessa.');

```

## Katso myös

- [PHP fwrite-funktio](https://www.php.net/manual/en/function.fwrite.php)
- [PHP fopen-funktio](https://www.php.net/manual/en/function.fopen.php)
- [PHP virheenkäsittely ja lokitiedostot](https://www.php.net/manual/en/book.errorfunc.php)