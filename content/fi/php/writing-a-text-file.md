---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
PHP:llä tekstitiedoston kirjoittaminen tarkoittaa tiedon tallentamista levylle. Koodarit tekevät tätä tiedon säilyttämiseen, lokien kirjaamiseen tai asetusten tallentamiseen.

## How to: - Kuinka:
```php
<?php
// Tallenna teksti tiedostoon
$tiedostonNimi = 'esimerkki.txt';
$tieto = "Hei! Tässä on esimerkkitietoa.\n";

// Kirjoita tiedostoon, append-moodi
file_put_contents($tiedostonNimi, $tieto, FILE_APPEND);

// Lue ja näytä tiedoston sisältö
echo file_get_contents($tiedostonNimi);
?>
```
Output:
```
Hei! Tässä on esimerkkitietoa.
```

## Deep Dive - Syväsukellus
Ennen PHP:tä, CGI-skriptit ja muut kielet hallitsivat dynaamista tiedostonkäsittelyä. PHP:n file_put_contents ja fopen-funktiot tarjoavat helpon tavan käsitellä tiedostoja. Alternatiiveina ovat tietokannat ja pilvipalvelut, mutta perinteinen tiedostoon kirjoittaminen on edelleen arvostettu toimintavarmuutensa ja yksinkertaisuutensa vuoksi.

## See Also - Katso Myös
- PHP.net:n dokumentaatio file_put_contents: https://www.php.net/manual/function.file-put-contents.php
- PHP:n tiedostonlukuopas: https://www.php.net/manual/en/function.fopen.php
- Stack Overflow: keskustelut ja kysymykset tiedostonkäsittelystä PHP:ssä
