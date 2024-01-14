---
title:                "PHP: Tarkistetaan löytyykö kansio"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa?

On monia syitä, miksi ohjelmoijien tulisi tarkistaa, onko hakemisto olemassa. Ensinnäkin, se auttaa varmistamaan, että ohjelma toimii odotetusti ja olemassa olevat hakemistot ovat tärkeitä monissa ohjelmoinnin käytännöissä. Lisäksi, jos ohjelma tarvitsee tallentaa tai hakea tietoja jostakin hakemistosta, on tärkeää tarkistaa sen olemassaolo ennen toimintojen suorittamista.

## Miten tarkistaa, onko hakemisto olemassa?

Tässä esimerkissä käytämme PHP-koodia tarkistaaksemme, onko hakemisto nimeltä "kuvat" olemassa ja tulostamme sen jälkeen viestin sen tilasta.

```PHP
<?php
$hakemiston_nimi = "kuvat";
if (file_exists($hakemiston_nimi)) {
  echo "Hakemisto " . $hakemiston_nimi . " on olemassa.";
} else {
  echo "Hakemistoa " . $hakemiston_nimi . " ei löytynyt.";
}
?>
```
Tämän koodin tulostuksena näet joko "Hakemisto kuvat on olemassa." tai "Hakemistoa kuvat ei löytynyt." riippuen hakemiston olemassaolosta.

## Syvempi sukellus hakemiston olemassaolon tarkistamiseen

Tarkistaessamme hakemiston olemassaoloa, PHP käyttää funktiota "file_exists", joka palauttaa toden, jos tiedosto tai hakemisto on olemassa. Huomaa, että tämä funktio toimii myös verkkotunnuksilla, joten voit myös tarkistaa, onko tietty verkkosivusto olemassa käyttämällä tätä samaa funktiota.

Lisäksi voit käyttää muita hyödyllisiä funktioita, kuten "is_dir", joka tarkistaa, onko tiedosto hakemisto, ja "is_file", joka tarkistaa, onko tiedosto normaali tiedosto.

## Katso myös

- [PHP:n virallinen dokumentaatio file_exists-funktiosta](https://www.php.net/manual/en/function.file-exists.php)
- [PHP:n virallinen dokumentaatio is_dir-funktiosta](https://www.php.net/manual/en/function.is-dir.php)
- [PHP:n virallinen dokumentaatio is_file-funktiosta](https://www.php.net/manual/en/function.is-file.php)