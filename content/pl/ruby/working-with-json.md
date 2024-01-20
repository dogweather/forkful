---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Praca z JSON (JavaScript Object Notation) to manipulowanie danymi w formacie używanym do wymiany informacji między systemami. Programiści wykorzystują JSON, bo to lekki, czytelny i elastyczny sposób na przechowywanie i przesyłanie strukturyzowanych danych.

## How to: (Jak to zrobić:)
```Ruby
require 'json'

# Przykład konwersji hasha na JSON
ruby_hash = { name: "Jan", age: 25, city: "Kraków" }
json_data = ruby_hash.to_json
puts json_data
# Output: {"name":"Jan","age":25,"city":"Kraków"}

# Przykład parsowania JSON na hash
json_string = '{"name":"Anna","age":28,"city":"Wrocław"}'
parsed_data = JSON.parse(json_string)
puts parsed_data
# Output: {"name"=>"Anna", "age"=>28, "city"=>"Wrocław"}
```

## Deep Dive (Dogłębna analiza)
JSON pojawił się w 2001 roku, wymyślony przez Douglasa Crockforda. Szybko stał się standardem we frontendzie (JavaScript) i backendzie (serwery, API). Alternatywą dla JSON jest XML, który jest bardziej rozbudowany i mniej przejrzysty. W Ruby operacje na JSON wykonywane są przy pomocy modułu `json`, który jest częścią standardowej biblioteki od wersji 1.9. Trzeba pamiętać, że Ruby używa symboli (np. `:name`), których JSON nie obsługuje, więc są one konwertowane na stringi.

## See Also (Zobacz również)
- JSON Schema: Jak definiować i walidować format JSON - [JSON Schema](https://json-schema.org/)
- JSON vs XML: Porównanie dwóch formatów - [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)