---
title:                "Työskentely TOML:n kanssa"
aliases:
- /fi/elixir/working-with-toml.md
date:                  2024-01-26T04:20:50.772412-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML:n kanssa työskentely tarkoittaa TOML-datam (Tom's Obvious, Minimal Language) jäsentämistä ja generointia käyttäen Elixiria. Ohjelmoijat käyttävät sitä asetustiedostojen käsittelyyn, koska TOML on luettavaa, helppo jäsentää ja kuvautuu hyvin hajautustietorakenteeseen.

## Miten:
Lisää ensin TOML-jäsentäjä mix-riippuvuuksiisi. Tässä esimerkissä käytetään `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Lue TOML-tiedosto:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Muuntaaksesi Elixir-datan TOML:ksi:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Esimerkkituloste:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Syväsukellus
TOML:n loi Tom Preston-Werner, GitHubin perustaja, käytettäväksi asetustiedostoissa. Sen on tarkoitus olla yksinkertaisempi kuin XML ja tiiviimpi kuin YAML säilyttäen samalla johdonmukaisuuden.

Vaihtoehtoihin kuuluvat JSON, YAML ja INI-tiedostot, joilla kaikilla on omat kompromissinsa ihmisselkeydessä ja tietorakenteen yhteensopivuudessa. TOML loistaa välilehtimäisten tietojen ja tietojen ryhmittelyn selkeässä esittämisessä.

Elixirissä TOML-käsittely riippuu koodaus- ja dekoodauskirjastoista, jotka muuntavat TOML-merkkijonot Elixirimapeiksi ja päinvastoin. Jäsentäminen toimii vastaamalla TOML:n syntaksisääntöihin ja muuttamalla ne Elixirim datatyypeiksi. Koodaus tekee päinvastaisen toiminnon määrittämällä Elixirim datatyypit takaisin päteviksi TOML-syntaksiksi.

## Katso Myös
- TOML-kieli: https://toml.io/en/
- `toml-elixir` GitHub-repositorio: https://github.com/bitwalker/toml-elixir
- Hex-paketin tiedot `toml-elixir`: https://hex.pm/packages/toml_elixir
