---
title:                "JSON-tiedostojen käsittely"
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
JSON, JavaScript Object Notation, on datanvaihtoformaatti. Rubyssa työskennellään JSONin kanssa koska se on helppolukuinen ja -kirjoitettava sekä yhteensopiva monien ohjelmointikielten kanssa.

## How to:
```Ruby
require 'json'

# JSON-muotoisen tekstin muuttaminen Ruby-olioksi
json_data = '{"name": "Matti", "age": 30, "city": "Helsinki"}'
ruby_hash = JSON.parse(json_data)
p ruby_hash
# Tulostaa: {"name"=>"Matti", "age"=>30, "city"=>"Helsinki"}

# Ruby-olion muuttaminen JSON-muotoiseksi tekstiksi
ruby_hash = { name: "Liisa", age: 25, city: "Espoo" }
json_data = ruby_hash.to_json
puts json_data
# Tulostaa: {"name":"Liisa","age":25,"city":"Espoo"}
```

## Deep Dive
JSON on alun perin kehitetty helpottamaan JavaScriptilla datan käsittelyä verkkosovelluksissa. Sen suosio laajeni muihin ohjelmointikieliin sen yksinkertaisuuden ja selkeyden ansiosta. Vaihtoehtoisia tiedostomuotoja ovat XML ja YAML, mutta JSON on suosittu etenkin REST API -rajapinnoissa läpi internetin. Rubyssa JSONin käsittely on tehty erittäin helpoksi standardikirjaston 'json' -gemillä, mutta on myös olemassa muita kirjastoja, kuten 'oj' (Optimized JSON), jotka tarjoavat suorituskyvyltään optimoituja vaihtoehtoja.

## See Also
- JSONin virallinen nettisivu: [json.org](https://www.json.org/json-en.html)
- 'oj' gem GitHubissa: [ohler55/oj](https://github.com/ohler55/oj)
