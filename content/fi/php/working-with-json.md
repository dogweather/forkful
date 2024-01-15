---
title:                "Työskentely jsonin kanssa"
html_title:           "PHP: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi JSON on tärkeä PHP-ohjelmoinnissa?

JSON, eli JavaScript Object Notation, on tärkeä osa PHP-ohjelmointia, sillä se tarjoaa yksinkertaisen ja tehokkaan tavan käsitellä ja välittää dataa eri järjestelmien välillä. JSON-muodossa oleva data on helppo lukea ja muokata sekä koneellisesti että ihmiselle.

## Miten JSONia käytetään PHP:ssa?

JSON dataa voidaan käsitellä PHP:ssa käyttäen useita eri funktioita, jotka ovat osa PHP:n sisäänrakennettua JSON-luokkaa. Tässä on esimerkki, miten JSON-muotoista dataa voidaan muuntaa PHP:n taulukoksi:

```PHP 
<?php
// JSON-muotoinen data
$json_data = '{"nimi":"Maija", "ikä":30, "harrastukset":["luistelu", "lukeminen", "käsityöt"]}';
// Muunna JSON-tiedot PHP-taulukoksi
$taulukko = json_decode($json_data);
// Tulosta taulukko
print_r($taulukko);
?>
```

Tämän koodin tulostustuloksena näet seuraavan:

```
Array (
    [nimi] => Maija
    [ikä] => 30
    [harrastukset] => Array (
        [0] => luistelu
        [1] => lukeminen
        [2] => käsityöt 
    )
)
```

## Syvällinen sukellus JSONin maailmaan

JSONin käyttö PHP:ssa on helppoa ja tehokasta, sillä PHP tarjoaa valmiita funktioita JSON-dataan käsittelyyn. Voit myös luoda uusia PHP-taulukoita ja muuntaa ne JSON-muotoon käyttämällä `json_encode`-funktiota.

JSON-muodossa olevan datan käyttö on myös yleinen tapa kommunikoida eri järjestelmien välillä, esimerkiksi verkkosivun ja tietokannan välillä. JSONia voidaan myös käyttää tiedostojen tallentamiseen ja lähettämiseen, mikä tekee siitä monipuolisen työkalun PHP-ohjelmoinnissa.

## Katso myös

- [PHP:n virallinen JSON-dokumentaatio](https://www.php.net/manual/en/book.json.php)
- [JSON-formaattisäännöt](https://www.json.org/json-fi.html)
- [JSON:n käyttö paikallisten tiedostojen kanssa PHP:ssa](https://www.php.net/manual/en/function.json-decode.php)