---
title:                "PHP: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit kirjoittaa tekstitiedoston PHP-ohjelmointikielellä? Se voi tuntua yksinkertaiselta tehtävältä, mutta tekstitiedostot ovat tärkeä osa ohjelmointia. Ne voivat sisältää tärkeää tietoa, jota tarvitaan ohjelman toiminnan kannalta.

## Miten

PHP:lla tekstitiedoston kirjoittaminen on helppoa. Voit käyttää fopen()-funktiota avataksesi tiedoston ja fwrite()-funktiota kirjoittaaksesi siihen haluamasi sisällön. Esimerkiksi:

```PHP
$file = fopen("tekstitiedosto.txt","w");
fwrite($file, "Tämä on tekstitiedoston sisältö.");
fclose($file);
```

Koodinpätkä avaa uuden tekstitiedoston nimeltä "tekstitiedosto.txt" ja tallentaa siihen annetun sisällön. fclose()-funktio sulkee tiedoston, kun kirjoittaminen on valmis.

## Syvemmälle

Tiedoston avaamisen yhteydessä fopen()-funktiolle on annettava myös tiedoston avausmuoto. "w" tarkoittaa kirjoitustilaa (write), mutta voit myös käyttää muita muotoja riippuen siitä, mitä haluat tehdä tiedostolle. Esimerkiksi "r" avaa tiedoston lukutilaan (read) ja "a" lisää uutta sisältöä tiedoston loppuun (append).

Voit myös käyttää fwrite()-funktion sijasta file_put_contents()-funktiota, joka kirjoittaa sisällön suoraan tiedostoon. Esimerkiksi:

```PHP
file_put_contents("tekstitiedosto.txt","Tämä on toinen tekstirivi.", FILE_APPEND);
```

Tässä tapauksessa tiedostoa ei tarvitse avata erikseen, vaan funktio luo uuden tiedoston, jos sellaista ei jo ole olemassa, ja lisää annetun sisällön loppuun.

## Katso myös

- PHP:n virallinen dokumentaatio tiedostojen käsittelyyn: https://www.php.net/manual/en/book.filesystem.php
- Kattava opas PHP:lla tekstitiedostojen käsittelyyn: https://www.w3schools.com/php/php_file.asp
- YouTube-video tekstitiedostojen käsittelystä PHP:lla: https://www.youtube.com/watch?v=8kZO8higbhE