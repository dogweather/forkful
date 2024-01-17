---
title:                "Töitä csv:n kanssa"
html_title:           "PHP: Töitä csv:n kanssa"
simple_title:         "Töitä csv:n kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-csv.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?
CSV (comma-separated values) on tietomuoto, jota käytetään tallentamaan taulukkomuotoista dataa, kuten tietokantojen sisältöjä tai Excel-tiedostoja. PHP-ohjelmoijat käyttävät CSV-tiedostoja lataamaan, lukemaan ja käsittelemään tietoja, jotka on tallennettu tähän muotoon.

# Kuinka:
Tässä esimerkkejä, kuinka käyttää PHP:ta CSV-tiedostojen kanssa.

**Lue CSV-tiedosto ja tulosta sen sisältö:**
```PHP
$file = fopen('tiedosto.csv', 'r');
while (($line = fgetcsv($file)) !== FALSE) {
  print_r($line);
}
fclose($file);
```
Tämä koodi avaa CSV-tiedoston, lukee sen sisällön rivi riviltä ja tulostaa jokaisen rivin taulukkona.

**Luo uusi CSV-tiedosto ja tallenna siihen tietoja:**
```PHP
$file = fopen('tiedosto.csv', 'w');
$data = array(
  array('Otsikko1', 'Otsikko2', 'Otsikko3'),
  array('Data1', 'Data2', 'Data3')
);
foreach ($data as $line) {
  fputcsv($file, $line);
}
fclose($file);
```
Tämä koodi luo uuden CSV-tiedoston ja tallentaa siihen taulukkona annetut tiedot. Jokainen taulukon rivi vastaa yhtä riviä CSV-tiedostossa.

# Syvemmälle:
CSV-tiedostoja käytetään laajasti erilaisten tietojen tallentamiseen ja siirtämiseen, ja ne ovat erityisen hyödyllisiä web-kehityksessä. CSV korvaa vanhemmat muodot, kuten DBF ja DIF, ja sen suosio johtuu sen yksinkertaisuudesta ja yhteensopivuudesta useiden ohjelmistojen kanssa.

On myös muita tapoja käsitellä taulukkomuotoista dataa PHP:ssa, kuten käyttämällä tietokantakyselyitä tai XML-tiedostoja. Mutta CSV-tiedostojen käyttö on edelleen suosittua erityisesti, kun tietoja halutaan jakaa muiden ohjelmien kanssa.

PHP:n avulla CSV-tiedostojen käsittely on helppoa ja nopeaa, mutta on tärkeää varmistaa tiedostojen oikea formaatti ja tietojen oikea muotoilu, jotta koodi toimii oikein.

# Katso myös:
- PHP:n virallinen dokumentaatio CSV-tiedostojen käsittelemisestä: https://www.php.net/manual/en/function.fgetcsv.php
- Mahdollisuus tallentaa tietoja tietokantoihin: https://www.php.net/manual/en/book.pdo.php
- XML-tiedostojen käsittely PHP:ssa: https://www.php.net/manual/en/book.xml.php