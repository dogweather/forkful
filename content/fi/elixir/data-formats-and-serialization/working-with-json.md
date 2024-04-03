---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:10.574590-07:00
description: "Kuinka: Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `Jason`-kirjastoa, joka\
  \ on suosittu valinta JSONin j\xE4sent\xE4miseen ja tuottamiseen. Lis\xE4\xE4 ensin\
  \ `Jason` projektisi\u2026"
lastmod: '2024-03-13T22:44:56.248147-06:00'
model: gpt-4-0125-preview
summary: "Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `Jason`-kirjastoa, joka on suosittu\
  \ valinta JSONin j\xE4sent\xE4miseen ja tuottamiseen."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

## Kuinka:
Elixirissä voit käyttää `Jason`-kirjastoa, joka on suosittu valinta JSONin jäsentämiseen ja tuottamiseen. Lisää ensin `Jason` projektisi riippuvuuksiin `mix.exs`-tiedostossa:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Suorita sitten `mix deps.get` riippuvuuden noutamiseksi.

### JSONin jäsentäminen:
Muuntaaksesi JSON-merkkijonon Elixirin tietorakenteiksi:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Tuloste: %{"name" => "John", "age" => 30}
```

### JSONin tuottaminen:
Muuntaaksesi Elixirin mapin JSON-merkkijonoksi:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Tuloste: {"age":25,"name":"Jane"}
```

### Structien kanssa työskentely:
Koodataksesi Elixirin structin, sinun on toteutettava `Jason.Encoder`-protokolla structillesi. Tässä on esimerkki:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Tuloste: {"age":28,"name":"Mike"}
```

Tämä yksinkertainen lähestymistapa auttaa sinua aloittamaan JSONin käsittelyn integroinnin Elixir-sovelluksiisi, helpottaen tiedonvaihtoa erilaisissa ohjelmointiympäristöissä.
