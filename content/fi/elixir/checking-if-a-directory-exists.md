---
title:                "Tarkistetaan, onko kansiolle olemassa"
html_title:           "Elixir: Tarkistetaan, onko kansiolle olemassa"
simple_title:         "Tarkistetaan, onko kansiolle olemassa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi haluaisit tarkistaa, onko hakemistoa olemassa. Yleisimpiä syitä ovat esimerkiksi ohjelman suorituksen yhteydessä tietojen tallentaminen tai hakemiston olemassaolon tarkastaminen varmistaaksesi, että se voidaan käyttää myöhemmin.

## Näin teet

Hakemiston olemassaolon tarkistaminen Elixirillä on helppoa käyttäen `File.exists?` -funktiota.

```elixir
iex> File.exists?("hakemisto/")
true
```

Funktio palauttaa boolean-arvon, joka ilmaisee onko hakemisto olemassa vai ei. Voit myös käyttää `File.dir?` -funktiota tarkistaaksesi onko kyseessä varmasti hakemisto.

```elixir
iex> File.dir?("hakemisto/")
true
```

Jos haluat tarkistaa olemassaolon alihakemistossa, voit lisätä sen polun perään.

```elixir
iex> File.exists?("hakemisto/alihakemisto/")
true
```

## Syvemmälle

Tarkistaessaan hakemiston olemassaolon, `File.exists?` käyttää `:file.read_dir` -funktiota, joka kutsuu käyttöjärjestelmän `readdir` -kutsua. Tämä tarkoittaa, että Elixirin `File.exists?` -funktio on yleensä tehokkaampi kuin vastaavat kutsut muissa ohjelmointikielissä.

## Katso myös

- [Elixirin virallinen dokumentaatio File-moduulista](https://hexdocs.pm/elixir/File.html)
- [Tietoa käyttöjärjestelmän readdir-kutsusta](https://linux.die.net/man/3/readdir)