---
title:                "Arbeta med JSON"
date:                  2024-02-03T19:24:02.752073-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

JSON (JavaScript Object Notation) är ett lättviktigt datautbytesformat, som är utbrett i webbapplikationer för dataväxling mellan klienter och servrar. Programmerare arbetar med JSON i Ruby för att tolka data som mottagits från externa källor eller för att formatera data för API-svar, och utnyttjar dess lättlästa struktur för enkel datamanipulation och lagring.

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
