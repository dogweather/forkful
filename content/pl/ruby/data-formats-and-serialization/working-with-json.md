---
title:                "Praca z JSON"
aliases:
- /pl/ruby/working-with-json.md
date:                  2024-02-03T19:24:13.522336-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

JSON (JavaScript Object Notation) to lekki format wymiany danych, powszechnie stosowany w aplikacjach internetowych do wymiany danych między klientami a serwerami. Programiści pracują z JSON w Ruby, aby parsować dane otrzymywane z zewnętrznych źródeł lub formatować dane dla odpowiedzi API, wykorzystując jego łatwo czytelną strukturę do łatwej manipulacji danymi i przechowywania.

## Jak to zrobić:

Ruby, ze swoją standardową biblioteką, oferuje bezproblemowe sposoby parsowania i generowania JSON. Podstawowym modułem do tych operacji jest `json`, który można łatwo zintegrować z dowolną aplikacją Ruby.

### Parsowanie JSON:

Aby przekonwertować ciąg JSON na hash Ruby, możesz użyć metody `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Wyjście: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generowanie JSON:

Odwrotnie, aby przekonwertować hash Ruby na ciąg JSON, używasz metody `JSON.generate` lub metody `to_json` dostępnej w obiektach Ruby po wymaganiu biblioteki `json`.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Wyjście: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Biblioteki stron trzecich:

Chociaż standardowa biblioteka Ruby obejmuje podstawowe obsługi JSON, wiele projektów polega na bibliotekach stron trzecich dla rozszerzonej funkcjonalności i wydajności. Popularnym wyborem jest `Oj` (Optimized JSON).

#### Parsowanie z Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Wyjście: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generowanie z Oj:

Oj oferuje również szybki sposób na generowanie JSON z obiektów Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Wyjście: {"name":"Samantha","age":35,"city":"Miami"}
```

Te przykłady ilustrują prostotę pracy z JSON w Ruby, czyniąc ją dostępną dla zadań od prostych manipulacji danymi do skomplikowanych komunikacji API.
