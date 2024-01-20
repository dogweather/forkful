---
title:                "Työskentely csv:n kanssa"
html_title:           "Gleam: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV on tietomuoto, jota ohjelmoijat käyttävät taulukkotietojen tallentamiseen ja jakamiseen. Se on lyhenne sanoista "Comma-Separated Values" ja se koostuu riveistä ja sarakkeista, jotka on eroteltu pilkulla. Olet varmasti törmännyt CSV-tiedostoihin esimerkiksi lataamalla Excel-taulukoita tai tuodessa dataa sovelluksiin.

Ohjelmoijat käyttävät CSV-muotoa, koska se on yksinkertainen ja helppo käsitellä. Sen avulla voidaan tallentaa suuriakin tietomääriä tehokkaasti ja helposti jakaa tietoa eri järjestelmien välillä.

## Miten:

Gleam-ohjelmoinnissa CSV-tiedostoja käsitellään kätevästi [csv-paketin](https://github.com/gleam-lang/csv) avulla. Ensimmäiseksi meidän täytyy lisätä paketin riippuvuus projektiimme käyttämällä `rebar3`, minkä jälkeen voimme aloittaa CSV-tiedoston käsittelyn.

```
{ ok, data } = File.read("data.csv")
|> csv.parse()
|> csv.row_vectors()
```

Yllä olevassa koodiesimerkissä käytämme `csv.parse()` -funktiota lukemaan ja parsimaan CSV-tiedoston sisältö muuttujaksi `data`. Tämän jälkeen `csv.row_vectors()` luo taulukon riveistä, jotka voidaan helposti käsitellä Gleamin taulukkopakettien avulla.

## Syventymistä:

CSV:tä on käytetty jo yli 50 vuotta ja se on edelleen erittäin suosittu tiedostoformaatti. Sen yksinkertaisuuden ansiosta sitä käytetään laajasti lähes kaikissa ohjelmointikielissä ja se on käytössä esimerkiksi tiedon siirrossa järjestelmien välillä.

Gleamin lisäksi CSV-tiedostoja voi käsitellä monella muullakin ohjelmointikielellä, kuten Pythonilla ja Javalla. Lisäksi on olemassa erilaisia CSV-paketteja ja kirjastoja, jotka tarjoavat erilaisia toimintoja CSV-tiedostojen käsittelyyn.

CSV-tiedostoon tallennettuja tietoja käsitellään Gleamissa sisäisesti binäärinä, mikä tekee tiedostojen käsittelystä nopeaa ja tehokasta.

## Katso myös:

- [CSV Wikipediassa](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSV-tiedostoja käsittelevät libraryt Pythonissa](https://docs.python.org/3/library/csv.html)