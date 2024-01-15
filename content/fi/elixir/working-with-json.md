---
title:                "Työskentely jsonin kanssa"
html_title:           "Elixir: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON on yksi yleisimmin käytetyistä tiedonsiirtomuodoista web-kehityksessä, ja Elixir-ohjelmointikielen avulla sen käsittely on nopeaa ja tehokasta.

## Miten

```Elixir
# Esimerkki JSON-tiedon lukemisesta ja muuttamisesta

# Avataan JSON-tiedosto ja tallennetaan sen sisältö muuttujaan
{:ok, json_data} = File.read("data.json")

# Muutetaan JSON-data Elixirin map-tietotyypiksi
json_map = Jason.decode!(json_data)

# Haetaan mapista tietty arvo käyttäen avain-arvo-paria
value = json_map["key"]

# Muutetaan Elixirin map takaisin JSON-muotoon
new_json = Jason.encode!(json_map)
```

### Tulostus:
```Elixir
# json_map
%{"key" => "value"}

# value
"value"

# new_json
"{\"key\":\"value\"}"
```

## Syventävä tutustuminen

Elixir tarjoaa monipuolisia työkaluja JSON-datan käsittelyyn, kuten sisäänrakennetut moduulit Jason ja Poison. Jason on nopeampi mutta rajoitetumpi, kun taas Poison mahdollistaa monimutkaisemman JSON-rakenteen käsittelyn. Lisäksi Elixir tarjoaa mahdollisuuden luoda omia JSON-parsereita tarvittaessa.

## Katso myös

- [Elixirin virallinen JSON-dokumentaatio](https://hexdocs.pm/jason/readme.html)
- [Poisonin GitHub-sivu](https://github.com/devinus/poison)