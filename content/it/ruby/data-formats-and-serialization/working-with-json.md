---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:54.508919-07:00
description: "Come fare: Ruby, con la sua libreria standard, fornisce modi semplici\
  \ per analizzare e generare JSON. Il modulo principale per queste operazioni \xE8\
  \ `json`,\u2026"
lastmod: '2024-03-13T22:44:44.072498-06:00'
model: gpt-4-0125-preview
summary: Ruby, con la sua libreria standard, fornisce modi semplici per analizzare
  e generare JSON.
title: Lavorare con JSON
weight: 38
---

## Come fare:
Ruby, con la sua libreria standard, fornisce modi semplici per analizzare e generare JSON. Il modulo principale per queste operazioni è `json`, che può essere facilmente integrato in qualsiasi applicazione Ruby.

### Analisi di JSON:
Per convertire una stringa JSON in un hash Ruby, puoi utilizzare il metodo `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Output: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Generazione di JSON:
Al contrario, per convertire un hash Ruby in una stringa JSON, si usa il metodo `JSON.generate` o il metodo `to_json` disponibile sugli oggetti Ruby una volta che la libreria `json` è richiesta.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Output: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Librerie di terze parti:
Sebbene la libreria standard di Ruby copra la gestione basica di JSON, molti progetti si affidano a librerie di terze parti per funzionalità avanzate e prestazioni migliori. Una scelta popolare è `Oj` (Optimized JSON).

#### Analisi con Oj:
```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Output: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generazione con Oj:
Oj offre anche un modo veloce per generare JSON da oggetti Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Output: {"name":"Samantha","age":35,"city":"Miami"}
```

Questi esempi illustrano la natura semplice del lavoro con JSON in Ruby, rendendolo accessibile per compiti che vanno da semplici manipolazioni di dati a complesse comunicazioni API.
