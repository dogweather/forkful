---
title:                "Elixir: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi
CSV eli Comma-Separated Values on yleinen tietojen tallennusmuoto, jota käytetään usein taulukkomuotoisissa tiedostoissa. Elixirin avulla CSV-tiedostojen käsittely on helppoa ja tehokasta. Se sopii erinomaisesti esimerkiksi datan analysointiin, raporttien luomiseen ja muihin liiketoiminnallisiin tehtäviin.

## Miten
Elixirilla on valmiina moduuli, joka helpottaa CSV-tiedostojen lukemista ja kirjoittamista. Voit lukea CSV-tiedoston käyttämällä `File.stream!`-funktiota ja `CSV`-moduulia, jonka jälkeen voit käsitellä tiedot `Stream`-funktion avulla. Esimerkiksi:

```Elixir
File.stream!("data.csv") |> CSV.parse |> Stream.map(fn [column1, column2, column3] -> {column1, column2, column3} end) |> Enum.to_list
```

Tämä koodi lukee tiedoston nimeltä "data.csv" ja muuntaa sen taulukoksi, jonka jälkeen se voidaan käsitellä esimerkiksi `Enum`-moduulin avulla. Voit myös luoda uuden CSV-tiedoston käyttämällä `CSV.encode`-funktiota ja antamalla sille haluamasi taulukon.

## Syventyminen
Elixirin `CSV`-moduulilla on paljon erilaisia toimintoja, joita voit käyttää CSV-tiedostojen käsittelyyn. Voit esimerkiksi määrittää erilaisia erottimia tai valita tiettyjä sarakkeita tiedostosta lukemisen yhteydessä. Moduulin dokumentaatiosta löydät lisätietoa eri toiminnoista ja niiden käytöstä.

## Katso myös
- [Elixirin dokumentaatio CSV-moduulista](https://hexdocs.pm/elixir/CSV.html)
- [Elixirin tietojen käsittely Streamien avulla](https://elixirschool.com/fi/lessons/advanced/stream-data-processing/)
- [CSV-tiedostojen käsittely Elixirillä](https://medium.com/flatiron-labs/elixir-stream-and-csv-a-life-changing-journey-3bdd718f3938) (englanniksi)