---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON är textformat för datalagring och kommunikation. Programmerare använder det för att enkelt utbyta data mellan olika språk och system.

## How to:
Här är ett enkelt sätt att använda JSON i Ruby.
```Ruby
require 'json'

# Skapa ett Ruby-objekt
person = { name: "Anna", age: 34, city: "Stockholm" }

# Konvertera objektet till en JSON-sträng
person_to_json = person.to_json
puts person_to_json

# Läs JSON-strängen i ett nytt Ruby-objekt
json_to_person = JSON.parse(person_to_json)
puts json_to_person["name"]  # Skriver ut: Anna
```
Output blir JSON-format: `{"name":"Anna","age":34,"city":"Stockholm"}` och sedan Ruby-objektet: Anna.

## Deep Dive
JSON (JavaScript Object Notation) togs fram i början av 2000-talet. Alternativ till JSON inkluderar XML och YAML. Ruby använder `json`-biblioteket, som följer `RFC 7159`. Parsing med `JSON.parse` och serialisering med `.to_json` är grundstommar i Ruby's hantering av JSON.

## See Also
- JSON-specifikationen: [json.org/json-sv.html](http://json.org/json-sv.html)
