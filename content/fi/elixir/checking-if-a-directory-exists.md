---
title:                "Tarkistetaan, onko hakemisto olemassa"
date:                  2024-01-19
simple_title:         "Tarkistetaan, onko hakemisto olemassa"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja miksi?
Tarkistetaan, onko hakemisto olemassa, ettei törmätä virheisiin yrittäessä käsitellä olematonta haksua. Se on tärkeä ennakkocheckki tiedostojen käsittelyssä ja datan tallentamisessa.

## How to: - Kuinka tehdään:
```elixir
File.dir?("some_directory")
```
Jos hakemisto `some_directory` on olemassa, palauttaa `true`, muutoin `false`.

Esimerkki käytännössä:
```elixir
# Luodaan hieman esimerkkidataa
File.mkdir("example_dir")

# Tarkistetaan, löytyykö litteroidusti
File.dir?("example_dir") 
# => true

File.dir?("fantasia_hakemisto")
# => false
```

## Deep Dive - Syväsukellus:
Elixir käyttää Erlangin virtuaalikonetta (BEAM) kaikelle tiedostojärjestelmän hallinnalle. Kun tarkistamme hakemiston olemassaolon, tehdään kysely käyttöjärjestelmälle BEAM:in kautta. Historiallisesti tiedostojen ja hakemistojen hallinta on ollut kriittinen osa ohjelmointia, ja Elixir mahdollistaa sen tehokkaasti funktionaalisella otteella.

Vaihtoehtoja `File.dir?/1`-funktiolle on esimerkiksi `:filelib.is_dir/1`, joka on Erlangiin sisäänrakennettu. On tärkeää ymmärtää, että Elixiriä kirjoitettaessa suoritamme lopulta Erlang-funktioita BEAM:ssä, joten Elixiriin ja Erlangiin liittyvä dokumentaatio on usein yhtä tärkeää.

Suorituskyky viime kädessä riippuu alla olevan käyttöjärjestelmän tiedostojärjestelmän hallinnasta, joten Elixir-ohjelmoija tekee parhaiten, kun ymmärtää myös perustason OS-tiedostojärjestelmän dynamiikkaa.

## See Also - Katso myös:
- Elixiriin liittyvä dokumentaatio `File`-moduulista: [hexdocs.pm](https://hexdocs.pm/elixir/File.html)
- Erlangin `:filelib`-moduulin dokumentaatio: [erlang.org](http://erlang.org/doc/man/filelib.html)
- Keskusteluja ja esimerkkikoodia tiedostojärjestelmästä Elixirissä: [elixirforum.com](https://elixirforum.com)
