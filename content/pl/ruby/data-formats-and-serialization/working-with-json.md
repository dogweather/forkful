---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:13.522336-07:00
description: "JSON (JavaScript Object Notation) to lekki format wymiany danych, powszechnie\
  \ stosowany w aplikacjach internetowych do wymiany danych mi\u0119dzy klientami\
  \ a\u2026"
lastmod: '2024-03-13T22:44:35.953560-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) to lekki format wymiany danych, powszechnie\
  \ stosowany w aplikacjach internetowych do wymiany danych mi\u0119dzy klientami\
  \ a serwerami."
title: Praca z JSON
weight: 38
---

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
