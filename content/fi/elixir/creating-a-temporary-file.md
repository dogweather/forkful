---
title:                "Luodaan tilapäistiedosto"
html_title:           "Elixir: Luodaan tilapäistiedosto"
simple_title:         "Luodaan tilapäistiedosto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi luoda väliaikaisia tiedostoja Elixirillä?

Joskus ohjelmointitehtävät vaativat tiedostojen käsittelyä, ja väliaikaiset tiedostot voivat olla hyödyllisiä tällaisissa tilanteissa. Elixirin avulla voit helposti luoda ja manipuloida väliaikaisia tiedostoja, mikä tekee siitä hyödyllisen työkalun monissa eri tilanteissa.

## Kuinka luoda väliaikaisia tiedostoja Elixirillä?

Elixirin `File`-moduuli tarjoaa useita toimintoja tiedostojen käsittelyyn, mukaan lukien väliaikaisen tiedoston luominen. Käytä `Tempfile.open/1`-funktiota määrittelemällä haluamasi tiedoston nimi ja kirjoita haluamasi sisältö `do`-lohkoon.

```elixir
file = Tempfile.open("temp.txt")
IO.write(file, "Tämä on väliaikainen tiedosto.")
```

Voit myös käyttää `Tempfile.stream/2`-funktiota luomaan virtuaalisen tiedoston ja kirjoittamaan siihen haluamasi tiedot.

```elixir
file = Tempfile.stream("temp.txt")
file |> Stream.into(File.stream!("original.txt"))
|> Enum.each(&IO.puts/1)
```

Molemmat näistä esimerkeistä luovat väliaikaisen tiedoston nimeltä "temp.txt" ja kirjoittavat siihen halutut tiedot. Muista sulkea luomasi tiedosto `File.close/1`-funktiolla, kun olet valmis käyttämään sitä.

## Syvempi sukellus väliaikaisten tiedostojen luomiseen

Elixirin väliaikaisten tiedostojen luomisen toimintoja kannattaa käyttää silloin, kun tarvitset hetkellisen tallennustilan tiedostoille ja haluat, että ne poistetaan automaattisesti käytön jälkeen. Väliaikaiset tiedostot luodaan usein `Tempfile.open/1`- tai `Tempfile.stream/2`-funktioiden avulla, mutta voit myös käyttää `File.mkstemp/2`-funktiota luomaan väliaikaisen tiedoston ja saamaan takaisin sen tiedostonumeron.

Elixirin `Tempfile`-moduuli tarjoaa myös muita hyödyllisiä toimintoja, kuten `Tempfile.copy/2`-funktion, jolla voit kopioida tiedostosta toiseen väliaikaisen tiedoston ja `Tempfile.open!/1`-funktion, joka heittää virheen, jos tiedoston luominen ei onnistu.

## Katso myös

- [Elixirin virallinen dokumentaatio tiedostojen käsittelystä](https://hexdocs.pm/elixir/File.html)
- [Elixirin `Tempfile`-moduulin virallinen dokumentaatio](https://hexdocs.pm/elixir/Tempfile.html)
- [Artikkeli väliaikaisista tiedostoista Elixirillä Hackernoonissa](https://hackernoon.com/elixir-quick-tip-temporary-files-b6a8b4787ac9)