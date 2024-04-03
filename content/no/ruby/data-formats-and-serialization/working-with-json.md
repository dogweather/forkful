---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:15.317061-07:00
description: "JSON (JavaScript Object Notation) er et lettvekts datautvekslingsformat,\
  \ utbredt i webapplikasjoner for datautveksling mellom klienter og servere.\u2026"
lastmod: '2024-03-13T22:44:41.355080-06:00'
model: gpt-4-0125-preview
summary: JSON (JavaScript Object Notation) er et lettvekts datautvekslingsformat,
  utbredt i webapplikasjoner for datautveksling mellom klienter og servere.
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Ruby, med sitt standardbibliotek, tilbyr sømløse måter å parse og generere JSON på. Hovedmodulen for disse operasjonene er `json`, som enkelt kan integreres i enhver Ruby-applikasjon.

### Parse JSON:
For å konvertere en JSON-streng til en Ruby-hash, kan du bruke `JSON.parse`-metoden.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Utdata: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generere JSON:
Tilsvarende, for å konvertere en Ruby-hash til en JSON-streng, bruker du `JSON.generate`-metoden eller `to_json`-metoden som er tilgjengelig på Ruby-objekter når `json`-biblioteket er påkrevd.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Utdata: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Tredjepartsbiblioteker:
Selv om Rubys standardbibliotek dekker grunnleggende JSON-håndtering, stoler mange prosjekter på tredjepartsbiblioteker for forbedret funksjonalitet og ytelse. Et populært valg er `Oj` (Optimized JSON).

#### Parse med Oj:
```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Utdata: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generere med Oj:
Oj tilbyr også en rask måte å generere JSON fra Ruby-objekter på:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Utdata: {"name":"Samantha","age":35,"city":"Miami"}
```

Disse eksemplene illustrerer den enkle naturen av å jobbe med JSON i Ruby, noe som gjør det tilgjengelig for oppgaver som varierer fra enkle datamanipulasjoner til komplekse API-kommunikasjoner.
