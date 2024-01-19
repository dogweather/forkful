---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tiedostossa olevan tekstin lukemista muistiin, käsiteltäväksi ja/tai muutettavaksi koodissa. Ohjelmoijat tekevät tämän yleisesti tiedon lukemiseksi ja kirjoittamiseksi tiedostoihin, jolloin sovellukset voivat säilyttää tai jakaa tietoja.

## Näin teet:

Voit lukea tiedoston Elixirillä `File.read/1` -funktiolla. Esimerkki:

```Elixir
{:ok, data} = File.read("example.txt")
IO.inspect(data)
```

Suorittamalla tämän koodin, luetaan "example.txt" tiedoston sisältö ja tulostetaan se.

## Syvempi tarkastelu

Elixirin `File.read/1` -funktio on saanut inspiraationsa perinteisistä UNIX-työkaluista, jotka tekevät tiedostojen käsittelystä selkeää ja tehokasta. 

Työkaluja tämän toteuttamiseksi on myös muita. Esimerkiksi `File.stream!/1` -funktio, joka voi lukea suuria tiedostoja pieninä palasina estämättä muita prosesseja.

`File.read/1` toteutus käyttää sisäisesti Erlangin `:file.read_file/1` -funktiota. Tämän ansiosta yhteensopivuus muiden BEAM-koneella toimivien kielten (kuten Erlang ja LFE) kanssa on taattu.

## Katso myös

- [Elixirin virallinen dokumentaatio - File module](https://hexdocs.pm/elixir/File.html)
- [Erlangin :file Module](http://erlang.org/doc/man/file.html)
- [The Pragmatic Bookshelf - Programming Elixir ≥ 1.6](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)