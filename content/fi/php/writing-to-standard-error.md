---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Standard error (stderr) on erikoistiedostovirta virheilmoitusten kirjoittamiseen. Ohjelmoijat käyttävät sitä erottaakseen tavallisen tulosteen (stdout) virheistä, mikä helpottaa tulosten käsittelyä ja debuggausta.

## How to: - Kuinka tehdä:

PHP:ssä `stderr` voi kirjoittaa `fopen('php://stderr', 'w')` avulla tai käyttämällä `STDERR` -vakioa, jos se on määritetty.

```php
<?php
// Käyttämällä fopen funktion kanssa
$fileDescriptor = fopen('php://stderr', 'w');
fwrite($fileDescriptor, "Virheilmoitus\n");
fclose($fileDescriptor);

// Suoraan STDERR:n kautta
fwrite(STDERR, "Suora stderr-virheilmoitus\n");
?>
```

Näytetuloste komentokehotteessa, kun ajat PHP-skriptin:
```
Virheilmoitus
Suora stderr-virheilmoitus
```

## Deep Dive - Syväsukellus:

Historiallisesti `stderr` on ollut UNIX-ja C-ohjelmoinnin käsite, jossa se on yksi kolmesta standardista tiedostovirrasta, mukaan lukien `stdin` ja `stdout`. PHP:ssä `STDERR` on määritetty esikäännetyksi tiedostonsisältäjäksi komentorivikäytössä, mutta se ei ole käytettävissä web-pohjaisessa ympäristössä. Vaihtoehtoisia tapoja kirjoittaa `stderr`:iin on harvoin tarpeen PHP:ssä, mutta lokeja ja poikkeustenhallintaa voidaan käyttää virheiden käsittelyyn korkeammalla tasolla.

## See Also - Katso Myös:

- PHP:n virallinen dokumentaatio `STDERR`: https://www.php.net/manual/en/features.commandline.io-streams.php
- PHP:n virallinen dokumentaatio virheenkäsittelyyn: https://www.php.net/manual/en/book.errorfunc.php
- Unix standard streams - Wikipedia: https://en.wikipedia.org/wiki/Standard_streams
