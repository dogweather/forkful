---
title:                "Työskentely csv:n kanssa"
html_title:           "PHP: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi käyttää CSV-tiedostoja PHP:ssä?

CSV (Comma-Separated Values) on yleinen tapa tallentaa taulukkodataa tekstitiedostoihin. Se on yksinkertainen ja helppokäyttöinen formaatti, joka mahdollistaa tietojen tallentamisen ja jakamisen eri ohjelmistojen välillä. PHP:llä voit helposti lukea ja kirjoittaa CSV-tiedostoja, mikä tekee siitä kätevän valinnan tietojen käsittelyyn.

## Kuinka käyttää CSV-tiedostoja PHP:ssä?

PHP:ssä on sisäänrakennettu toiminto `fgetcsv()`, joka lukee yhden rivin CSV-tiedostosta ja palauttaa sen taulukkona. Voit käyttää tätä funktiota yhdessä `fopen()`-funktion kanssa avataksesi tiedoston ja lukeaksesi sen sisällön. Jos haluat luoda uuden CSV-tiedoston, voit käyttää `fputcsv()`-funktiota, joka kirjoittaa taulukon tiedostoon CSV-formaatissa.

Esimerkiksi, jos haluat lukea ja tulostaa kaikki tiedoston sisältämät rivit, voit käyttää seuraavaa koodia:

```PHP
$file = fopen("tiedosto.csv", "r"); // Avaa tiedosto lukutilaan
while (($data = fgetcsv($file)) !== FALSE) { // Lukee yhden rivin kerrallaan
    print_r($data); // Tulostaa rivin taulukkona
}
fclose($file); // Sulkee tiedoston
```

Tämä koodi tulostaa jokaisen rivin erillisenä taulukkona, jota voi käyttää haluamallaan tavalla.

## Syvällä CSV-tiedostojen käsittelyssä

CSV-tiedostojen kanssa työskennellessä on tärkeää ottaa huomioon muutamia asioita. Ensinnäkin, varmista että tiedostossa käytetään oikeaa erotinta ja viittaajana toimii oikea merkkijono (``,` `,`|` jne.). Voit määrittää nämä asetukset käyttämällä `fgetcsv()`-funktion optiona `delimiter` ja `enclosure` parametreja.

Toiseksi, muista tarkistaa että tiedostossa ei ole virheitä ennen sen käsittelyä. Voit käyttää `fgetcsv()`-funktiota yhdessä `feof()`-funktion kanssa tarkistaaksesi, onko kaikki rivit luettu loppuun.

Lisäksi, jos haluat luoda uuden CSV-tiedoston, on tärkeää tarkistaa että taulukon jokainen rivi on sama pituus. Muussa tapauksessa tiedostoon tallennettaessa voi ilmetä virheitä.

## Katso myös

- [PHP fgetcsv() -funktio](https://www.php.net/manual/en/function.fgetcsv.php)
- [PHP fputcsv() -funktio](https://www.php.net/manual/en/function.fputcsv.php)
- [PHP fopen() -funktio](https://www.php.net/manual/en/function.fopen.php)
- [PHP feof() -funktio](https://www.php.net/manual/en/function.feof.php)