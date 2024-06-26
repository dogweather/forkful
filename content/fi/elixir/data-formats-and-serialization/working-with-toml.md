---
date: 2024-01-26 04:20:50.772412-07:00
description: "Miten: Lis\xE4\xE4 ensin TOML-j\xE4sent\xE4j\xE4 mix-riippuvuuksiisi.\
  \ T\xE4ss\xE4 esimerkiss\xE4 k\xE4ytet\xE4\xE4n `toml-elixir`."
lastmod: '2024-03-13T22:44:56.250197-06:00'
model: gpt-4-0125-preview
summary: "Lis\xE4\xE4 ensin TOML-j\xE4sent\xE4j\xE4 mix-riippuvuuksiisi."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

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
