---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "PHP: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary filejen luominen on tärkeää PHP-ohjelmoinnissa, koska se mahdollistaa väliaikaisen datan tallentamisen ja käsittelyn. Tämä voi olla hyödyllistä esimerkiksi lataamisen tai tallentamisen yhteydessä.

## Miten tehdä

Temporary filejen luominen PHP:ssa on helppoa ja vaivatonta. Se voidaan tehdä käyttäen `tmpfile()`-funktiota, joka luo uuden temporary filen palauttaen sen handle-tiedon. Tämä handle-tieto voidaan tallentaa muuttujaan ja käyttää myöhemmin tiedoston käsittelyssä. Katso alla oleva esimerkki:

```PHP
$file_handle = tmpfile();
fwrite($file_handle, "Tämä teksti tallentuu temporary fileen.");
rewind($file_handle);
echo fread($file_handle, filesize($file_handle));
fclose($file_handle);
```

Esimerkissä käytetään `tmpfile()`-funktiota luomaan uusi temporary file. Seuraavaksi käytetään `fwrite()`-funktiota kirjoittamaan teksti tiedostoon ja `rewind()`-funktiota asettamaan tiedoston lukupää alkuun. Tämän jälkeen `fread()`-funktiolla luetaan tiedoston sisältö ja lopuksi tiedosto suljetaan `fclose()`-funktiolla.

Temporary filejen käyttäminen on hyödyllistä esimerkiksi silloin, kun halutaan tallentaa käyttäjän syöttämää dataa väliaikaisesti. Näin ei tarvitse heti tallentaa dataa tietokantaan tai pysyvään tiedostoon. Lisäksi temporary filejen käyttäminen vähentää tarpeetonta tiedon käsittelyä ja puhdistaa tarpeettoman tiedon järjestelmästä.

## Syvempää tietoa

Temporary filejen luominen PHP:ssa tapahtuu käyttäen järjestelmän temp-hakemistoa. Tämä hakemisto on oletuksena asetettu ja sen sijainti vaihtelee käyttöjärjestelmän mukaan. Temp-hakemistoon tallennetaan väliaikaisia tiedostoja, jotka poistetaan kun niitä ei enää tarvita.

On myös mahdollista luoda temporary fileja omiin hakemistoihin käyttäen esimerkiksi `sys_get_temp_dir()`-funktiota. Tämä voi olla hyödyllistä, jos halutaan luoda temporary file johonkin tiettyyn paikkaan järjestelmässä.

Temporary filejen käyttäminen tuo myös tietoturvaetuja. Kun temporary file poistetaan, sen sisältö häviää ja estää mahdollisten tiedonkalasteluyritysten onnistumisen.

## Katso myös

- [PHP'n tmpfile()-funktio](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP'n fwrite()-funktio](https://www.php.net/manual/en/function.fwrite.php)
- [Temp-hakemisto käyttöjärjestelmästä riippuen](https://en.wikipedia.org/wiki/Temporary_folder)
- [PHP manualin osio väliaikaisista tiedostoista](https://www.php.net/manual/en/book.tempfile.php)