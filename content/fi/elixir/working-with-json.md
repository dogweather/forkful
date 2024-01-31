---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) on dataformaatti tiedon tallentamiseen ja siirtämiseen. Elixir-koodarit käsittelevät JSONia API-kommunikoinnissa ja config-tiedostoissa, koska se on kevyt ja helposti luettavissa ihmisille sekä koneille.

## How to:
Elixirissä JSONin käsittely onnistuu `Jason`-kirjaston avulla. Asenna kirjasto lisäämällä `{:jason, "~> 1.0"}` mix.exs-tiedoston dependency-listaan ja aio sitä.

```elixir
# Lisää Rosdep Jason
defp deps do
  [{:jason, "~> 1.0"}]
end
```
Koodissa deserialisoidaan JSON-stringi Elixir-mapseiksi ja serialisoidaan mappeja JSON-stringiksi.

```elixir
# JSONin purkaminen
json_string = "{\"key\":\"value\"}"
{:ok, result} = Jason.decode(json_string)
IO.inspect(result) # Tulostaa: %{"key" => "value"}

# JSONin luominen
map_data = %{"another_key" => "another_value"}
{:ok, json_output} = Jason.encode(map_data)
IO.puts(json_output) # Tulostaa: {"another_key":"another_value"}
```

## Deep Dive
JSON on syntynyt JavaScriptista, mutta nykyään sitä käytetään laajalti kieliriippumattomana tiedonsiirtomuotona. Jotkut vaihtoehdot JSONille ovat XML, YAML ja Protobuf, joilla jokaisella on omat etunsa ja käyttötilanteensa. Elixirissa `Jason` on yksi suosituimmista JSON-kirjastoista, mutta muitakin, kuten `Poison` ja `jsx`, on olemassa.

## See Also
- Elixirin virallinen sivu: [https://elixir-lang.org/](https://elixir-lang.org/)
- Jason GitHubissa: [https://github.com/michalmuskala/jason](https://github.com/michalmuskala/jason)
