---
title:                "Gleam: Työskentely csv-tiedostojen kanssa"
simple_title:         "Työskentely csv-tiedostojen kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi käyttää CSV-muotoa Gleam-ohjelmoinnissa?

CSV-muoto on yksi yleisimmin käytetystä tiedostomuodosta, joka sisältää tietoja taulukkoina. Tästä syystä CSV-muotoa käytetään laajalti eri ohjelmointikielissä, mukaan lukien Gleam. CSV-tiedostojen käsittely on välttämätöntä monissa sovelluksissa, joten on hyödyllistä oppia käyttämään niitä Gleam-ohjelmoinnissa.

## Kuinka työskennellä CSV-muodon kanssa Gleamissa?

Gleamilla on sisäänrakennettu CSV-moduuli, joka mahdollistaa CSV-tiedostojen käsittelyn. Ensimmäiseksi sinun täytyy tuoda CSV-moduuli käyttöösi "import" -lauseella ja antaa sille nimi "csv". Tämän jälkeen voit lukea CSV-tiedostoja käyttäen "csv.read" -funktiota, joka ottaa parametreina tiedoston nimen ja erotinmerkin.

```Gleam
import csv

let tiedosto = "tiedosto.csv"    //tiedoston nimi
let erotin = ","    //tiedoston erotinmerkki
let tulokset = csv.read(tiedosto, erotin)    //csv.read-funktio
```

Gleam-koodi lukee tiedoston ja tallentaa sen "tulokset" -muuttujaan. Tämän jälkeen voit käyttää "tulokset" -muuttujan arvoja ja tulostaa ne esimerkiksi konsoliin käyttäen "debug" -moduulia.

```Gleam
import debug    //debug-moduulin tuonti käyttöön

debug.inspect(tulokset)    //tulostaa CSV-tiedoston sisällön
```

Tässä esimerkissä käytettiin yksinkertaista CSV-tiedostoa, joka sisältää vain yhden rivin ja kolme arvoa erotettuna pilkulla. Monimutkaisempien CSV-tiedostojen käsittely vaatii ehkä hieman enemmän koodia, mutta periaate pysyy samana.

## Syvällisempi sukellus CSV:n maailmaan

CSV-tiedostoissa on yleensä otsikkorivi, joka sisältää sarakeotsikot. Gleamissa voit käyttää "csv.read_with_headers" -funktiota, joka lukee tiedoston samaan tapaan kuin "csv.read" -funktio, mutta palauttaa tulosrivit taulukkona, jossa indexit ovat sarakeotsikoita.

```Gleam
let otsikot = csv.read_with_headers(tiedosto, erotin)    //otsikoiden luku
debug.inspect(otsikot[0])    //tulostaa ensimmäisen tulosrivin
```

Tämä mahdollistaa tiedoston rivien käsittelyn selkeämmin käyttäen myös otsikkoja. Lisäksi, jos tiedostossasi on tyhjiä arvoja, voit käyttää "csv.read_with_headers_and_empty_cells" -funktiota, joka palauttaa myös tyhjät arvot taulukossa.

CSV-tiedostojen kirjoittaminen on myös mahdollista Gleamilla käyttäen "csv.write" -funktiota, joka ottaa parametreina tiedostonimen, tiedoston sisällön taulukkona ja erotinmerkin.

```Gleam
let sisalto = [["1", "2", "3"], ["4", "5", "6"]]    //tiedoston sisältö taulukossa
csv.write("uusi_tiedosto.csv", sisalto, erotin)    //uuden tiedoston luonti
```

## Katso myös

- [Gleam CSV-moduulin dokumentaatio](https://gleam.run/modules/csv.html)
- [CSV-tied