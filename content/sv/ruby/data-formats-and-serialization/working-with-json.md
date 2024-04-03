---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:02.752073-07:00
description: "Hur: Ruby, med sitt standardbibliotek, erbjuder s\xF6ml\xF6sa s\xE4\
  tt att tolka och generera JSON. Den prim\xE4ra modulen f\xF6r dessa operationer\
  \ \xE4r `json`, som enkelt\u2026"
lastmod: '2024-03-13T22:44:38.453231-06:00'
model: gpt-4-0125-preview
summary: "Ruby, med sitt standardbibliotek, erbjuder s\xF6ml\xF6sa s\xE4tt att tolka\
  \ och generera JSON."
title: Arbeta med JSON
weight: 38
---

## Hur:
Ruby, med sitt standardbibliotek, erbjuder sömlösa sätt att tolka och generera JSON. Den primära modulen för dessa operationer är `json`, som enkelt kan integreras i vilken Ruby-applikation som helst.

### Tolka JSON:
För att konvertera en JSON-sträng till en Ruby-hash kan du använda metoden `JSON.parse`.

```ruby
require 'json'

json_strang = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_strang)

puts ruby_hash
# Utdata: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generera JSON:
Tvärtemot, för att omvandla en Ruby-hash till en JSON-sträng, använder du metoden `JSON.generate` eller metoden `to_json` som är tillgänglig på Ruby-objekt när `json`-biblioteket är inkluderat.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_strang = ruby_hash.to_json

puts json_strang
# Utdata: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Tredjepartsbibliotek:
Även om Rubys standardbibliotek täcker grundläggande hantering av JSON, förlitar sig många projekt på tredjepartsbibliotek för förbättrad funktionalitet och prestanda. Ett populärt val är `Oj` (Optimized JSON).

#### Tolka med Oj:
```ruby
require 'oj'

json_strang = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_strang)

puts ruby_hash
# Utdata: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generera med Oj:
Oj erbjuder också ett snabbt sätt att generera JSON från Ruby-objekt:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_strang = Oj.dump(ruby_hash)

puts json_strang
# Utdata: {"name":"Samantha","age":35,"city":"Miami"}
```

Dessa exempel illustrerar det raka sättet att arbeta med JSON i Ruby, vilket gör det tillgängligt för uppgifter som varierar från enkla datamanipulationer till komplexa API-kommunikationer.
