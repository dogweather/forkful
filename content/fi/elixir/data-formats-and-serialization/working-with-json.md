---
title:                "Työskentely JSON:n kanssa"
aliases: - /fi/elixir/working-with-json.md
date:                  2024-02-03T19:22:10.574590-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

JSONin käsittelyyn kuuluu JSON-muotoiltujen merkkijonojen jäsentäminen tietorakenteiksi, joita Elixir voi käsitellä, ja Elixir-tietorakenteiden serialisointi takaisin JSON-merkkijonoiksi. Tämä on olennaista verkkokehityksessä, API:ssa ja konfiguraatiotiedostoissa, sillä JSON on kevyt, tekstipohjainen, kieliriippumaton tiedonvaihtoformaat, jota käytetään laajalti sen yksinkertaisuuden ja ihmisen luettavuuden vuoksi.

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
