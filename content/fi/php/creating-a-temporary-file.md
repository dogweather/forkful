---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Väliaikaistiedoston luominen on tekniikka, jossa ohjelmoija luo tiedoston, jonka tarkoituksena on joko säilyttää tietoja lyhyeksi aikaa tai toimia tiedonsiirron välittäjänä. Tätä tehdään yleisesti suurten datamäärien käsittelyssä tai monimutkaisten laskentojen väliaikaisessa tallennuksessa.

## Näin teet:

Alla on esimerkki väliaikaisen tiedoston luomisesta PHP:ssa.

```PHP
<?php

$temp_file = tmpfile();

$data = "Tämä on väliaikaista tietoa!";

fwrite($temp_file, $data);

rewind($temp_file);

echo fread($temp_file, 1024);

fclose($temp_file);

?>
```

Tuloste tulisi näyttää seuraavalta:

```
Tämä on väliaikaista tietoa!
```

## Syvempi sukellus

Väliaikaistiedostojen historiassa ohjelmoijat ovat käyttäneet niitä jo vuosikymmenien ajan monissa eri ohjelmointikielissä. PHP:n tapauksessa 'tmpfile()' -funktio on ollut olemassa jo sen varhaisista versioista lähtien, tarjoten tavan käsitellä helposti tiedostoja, jotka eivät vaadi pysyvää tallennustilaa.

Käytettäessä upotettuja tietokantoja tai muistinvaraisia tietokantoja kuten SQLite tai Redis, voi väliaikaistiedoston luonti tulla tarpeettomaksi. Nämä tietokannat mahdollistavat datan säilyttämisen nopeasti ja tehokkaasti ilman tarvetta tiedostoille. Kuitenkin, joissain tapauksissa tiedostojen avulla hallittu datan tallennus voi olla parempi vaihtoehto.

PHP:n `tmpfile()` palauttaa tiedoston kahvan väliaikaiseen tiedostoon. Tämä tiedosto poistetaan automaattisesti kun kahva suljetaan tai skriptin suorittaminen päättyy. Jos haluat säilyttää tiedoston, voit käyttää `tempnam()`-funktiota, joka luo väliaikaistiedoston antamaasi hakemistoon.

## Katso myös:

* PHP:n virallinen dokumentaatio `tmpfile`-funktiosta: https://www.php.net/manual/en/function.tmpfile.php
* Stackoverflow-keskustelu väliaikaistiedostojen käytöstä: https://stackoverflow.com/questions/3391811/how-do-i-use-a-temporary-file-in-my-php-script
* Tietoa muistinvaraisista tietokannoista: https://www.sqlite.org/inmemorydb.html