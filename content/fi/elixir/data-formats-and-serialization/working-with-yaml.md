---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:15.424696-07:00
description: "YAML, lyhenne sanoista YAML Ain't Markup Language, on ihmisen luettavissa\
  \ oleva datan sarjallistamisstandardi, jota k\xE4ytet\xE4\xE4n yleisesti\u2026"
lastmod: '2024-02-25T18:49:53.220545-07:00'
model: gpt-4-0125-preview
summary: "YAML, lyhenne sanoista YAML Ain't Markup Language, on ihmisen luettavissa\
  \ oleva datan sarjallistamisstandardi, jota k\xE4ytet\xE4\xE4n yleisesti\u2026"
title: "Ty\xF6skentely YAML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML, lyhenne sanoista YAML Ain't Markup Language, on ihmisen luettavissa oleva datan sarjallistamisstandardi, jota käytetään yleisesti konfiguraatiotiedostoissa ja datan vaihdossa eri kielten välillä, joilla on erilaiset datarakenteet. Ohjelmoijat käyttävät sitä sen yksinkertaisuuden ja kyvyn helposti esittää monimutkaisia hierarkisia tietoja takia.

## Miten:

Elixir ei sisällä sisäänrakennettua tukea YAML:lle. Voit kuitenkin käyttää kolmannen osapuolen kirjastoja, kuten `yamerl` tai `yaml_elixir`, työskennelläksesi YAML:n kanssa. Tässä keskitymme `yaml_elixir`-kirjastoon sen käytön helppouden ja kattavien ominaisuuksien vuoksi.

Lisää ensin `yaml_elixir` mix.exs-riippuvuuksiisi:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Suorita sitten `mix deps.get` hakeaksesi uuden riippuvuuden.

### YAML:n lukeminen

Oletetaan, että sinulla on yksinkertainen YAML-tiedosto, `config.yaml`, joka näyttää tältä:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

Voit lukea tämän YAML-tiedoston ja muuntaa sen Elixir-mappiin näin:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Esimerkki käyttö
Config.read()
# Tuloste: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### YAML:n kirjoittaminen

Mapin kirjoittaminen takaisin YAML-tiedostoon:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Esimerkki käyttö
ConfigWriter.write()
# Tämä luo tai ylikirjoittaa `new_config.yaml`-tiedoston määritellyllä sisällöllä
```

Huomaa, kuinka `yaml_elixir` mahdollistaa suoraviivaisen käännöksen YAML-tiedostojen ja Elixir-tietorakenteiden välillä, tehden siitä erinomaisen valinnan Elixir-ohjelmoijille, jotka tarvitsevat työskennellä YAML-datan kanssa.
