---
title:                "Työskentely csv:n kanssa"
html_title:           "Elixir: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi CSV:n kanssa kannattaa työskennellä?

CSV (Comma-Separated Values) on yleisesti käytetty tiedostomuoto tietojen tallentamiseen ja jakamiseen. Se on helppo lukea ja kirjoittaa, ja sen avulla on mahdollista käsittää suuria määriä tietoa tehokkaasti. Elixirin avulla CSV:llä työskentely on myös vaivatonta ja tehokasta, joten se on erittäin hyvä valinta mille tahansa ohjelmistokehittäjälle, joka tarvitsee työkaluja tiedon käsittelyyn.

## Kuinka työskennellä CSV:n kanssa

Elixir tarjoaa `CSV`-moduulin, jolla voi lukea ja kirjoittaa CSV-tiedostoja. Alla on esimerkki, miten voit lukea CSV-tiedoston käyttäen `File.stream!`-funktiota ja `CSV`-moduulia:

```Elixir
File.stream!("tiedosto.csv")
|> Stream.map(&CSV.decode_stream/1)
|> Stream.map(&Enum.to_list/1)
```

Tämä koodi lukee CSV-tiedoston ja palauttaa listoja, jotka sisältävät jokaisen rivin tiedot. Voit myös kirjoittaa CSV-tiedoston käyttäen `CSV.encode`-funktiota:

```Elixir
CSV.encode([["Sarake 1", "Sarake 2"], ["Arvo 1", "Arvo 2"]])
```

Tämä tuottaa seuraavan tulosteen:

```
"Sarake 1","Sarake 2"
"Arvo 1","Arvo 2"
```

## Syvemmälle CSV:n maailmaan

CSV-tiedostossa on usein tietoa, joka ei ole vain yksinkertaisia arvoja ja sarakeotsikoita. Elixirin `CSV`-moduuli tukee myös monimutkaisempia tietorakenteita, kuten listoja ja hajautustaloja. Alla on esimerkki, miten voit lukea CSV-tiedoston ja käyttää sitä Elixirin `Enum`-moduulin kanssa:

```Elixir
CSV.decode_file!("tiedosto.csv")
|> Enum.map(&List.to_tuple/1)
|> Enum.each(fn({otsikko, arvot}) -> IO.puts "#{otsikko}: #{arvot}" end)
```

Tässä ensin dekoodataan CSV-tiedosto ja muutetaan jokainen rivi tupleiksi, jonka jälkeen käydään läpi jokainen tuple ja tulostetaan otsikko ja siihen liittyvät arvot.

## Katso myös

- [Elixirin virallinen dokumentaatio CSV:lle](https://hexdocs.pm/elixir/CSV.html)
- [CSV: n tiedostomuodon spesifikaatiot](https://tools.ietf.org/html/rfc4180)
- [CSV-tiedostojen manipulointi Elixirissä](https://kaisersblog.blogspot.com/2018/09/working-with-csv-files-in-elixir.html)