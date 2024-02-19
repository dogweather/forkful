---
aliases:
- /fi/elixir/reading-a-text-file/
date: 2024-01-20 17:54:09.803081-07:00
description: "Lukemalla tekstitiedostoa ohjelma voi k\xE4sitell\xE4 tiedon sis\xE4\
  lt\xF6\xE4. T\xE4m\xE4 on peruskauraa, koska tiedot tallennetaan ja jaetaan usein\
  \ tekstitiedostoina."
lastmod: 2024-02-18 23:09:07.292440
model: gpt-4-1106-preview
summary: "Lukemalla tekstitiedostoa ohjelma voi k\xE4sitell\xE4 tiedon sis\xE4lt\xF6\
  \xE4. T\xE4m\xE4 on peruskauraa, koska tiedot tallennetaan ja jaetaan usein tekstitiedostoina."
title: Tekstitiedoston lukeminen
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)

Lukemalla tekstitiedostoa ohjelma voi käsitellä tiedon sisältöä. Tämä on peruskauraa, koska tiedot tallennetaan ja jaetaan usein tekstitiedostoina.

## How to: (Kuinka tehdä:)

Elixiriä käyttäen tekstitiedoston lukeminen:

```elixir
# Lue koko tiedosto kerralla
content = File.read!("example.txt")
IO.inspect(content)

# Lue tiedosto rivi riviltä
File.stream!("example.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

Esimerkin tuloste:

```
"Terve! Tämä on esimerkkitiedosto.\nSeuraava rivi alkaa tästä..\n"
Terve! Tämä on esimerkkitiedosto.
Seuraava rivi alkaa tästä..
```

## Deep Dive (Syväsukellus)

Tekstitiedoston lukeminen on peräisin aikojen alusta, kun tietokoneet alkoivat muokata ja tallentaa tietoa. 

Elixirissä `File.read!/1` on helppo tapa lukea tiedostoja, koska se palauttaa koko sisällön merkkijonona. Jos tiedosto on iso tai haluat prosessoida sisältöä rivi kerrallaan, käytä `File.stream!/1`, joka palauttaa enumerablen.

Vaihtoehtoina voisi käyttää myös kolmannen osapuolen kirjastoja, mutta vakiokeinot toimivat hyvin ja ovat suoraviivaisia.

Tarkkana pitää olla virhekäsittelyn kanssa. `File.read!/1` heittää virheen, jos tiedoston lukemisessa on ongelma. `File.read/1` palauttaa tuple-rakenteen jossa on `:ok` tai `:error`.

## See Also (Katso Myös)

- Elixirin virallinen dokumentaatio tiedoston käsittelystä: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Elixir Forum, keskustelua tiedostonlukemisesta: [https://elixirforum.com/](https://elixirforum.com/)
- "Programming Elixir" kirja, josta saa syvempää tietoa kielen paradigmoista, mukaan lukien tiedoston käsittely.
