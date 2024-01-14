---
title:    "PHP: Tiedoston kirjoittaminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monet PHP-ohjelmoijat käyttävät tekstitiedostoja tallentaakseen tietoa, kuten käyttäjien syötteitä tai konfiguraatiotietoja. Tässä blogikirjoituksessa käymme läpi, mitä text-tiedostojen kirjoittaminen PHP:lla tarkoittaa ja miten se on hyödyllistä.

## Miten

PHP tarjoaa käytännöllisen tavan kirjoittaa ja tallentaa dataa tekstitiedostoon `fwrite()`-funktion avulla. Alla olevassa esimerkissä näytämme, miten voit kirjoittaa merkkijonon tekstitiedostoon käyttämällä PHP:ta.

```PHP
<?php
// Avaamme tekstitiedoston kirjoitustilassa
$file = fopen('tiedosto.txt', 'w');

// Kirjoitetaan merkkijono tiedostoon
fwrite($file, 'Hei Maailma!');

// Suljetaan tiedosto
fclose($file);
?>
```

Yllä olevan koodin suorittamisen jälkeen tiedostoon `tiedosto.txt` on tallentunut teksti "Hei Maailma!".

## Syventävä sukellus

PHP-ohjelmoijilla on usein tarve tallentaa tietoa, kuten käyttäjien syötteitä tai laskettuja lukuja, johon he voivat palata myöhemmin. Tekstitiedostot ovat yksinkertainen tapa tallentaa tällaista dataa ja niiden käyttö on helppoa PHP:ssa.

On tärkeää muistaa avata tiedosto kirjoitustilassa käyttäen `fopen()`-funktiota ja sulkea se `fclose()`-funktiolla, jotta tiedostoon kirjoittaminen onnistuu. Lisäksi tekstitiedoston nimi ja polku voidaan määrittää `fopen()`-funktion ensimmäiseksi parametriksi sekä valita kirjoitustila (`'w'`).

Kun tallennettava tieto on määritelty, se voidaan antaa `fwrite()`-funktiolle toisena parametrina. Tämän jälkeen tiedosto on suljettava `fclose()`-funktiolla, jotta kaikki muutokset tallentuvat.

## Katso myös

- PHP:n `fwrite()`-funktio: https://www.php.net/manual/en/function.fwrite.php
- Tekstitiedostojen kirjoittaminen PHP:lla: https://www.w3schools.com/php/php_file_write.asp
- Tekstiedostojen käsittely PHP:lla: https://www.php.net/manual/en/book.filesystem.php