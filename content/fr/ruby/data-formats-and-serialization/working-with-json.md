---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:02.506277-07:00
description: "Comment faire : Ruby, avec sa biblioth\xE8que standard, offre des moyens\
  \ transparents pour analyser et g\xE9n\xE9rer du JSON. Le module principal pour\
  \ ces\u2026"
lastmod: '2024-03-13T22:44:58.443486-06:00'
model: gpt-4-0125-preview
summary: "Ruby, avec sa biblioth\xE8que standard, offre des moyens transparents pour\
  \ analyser et g\xE9n\xE9rer du JSON."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
Ruby, avec sa bibliothèque standard, offre des moyens transparents pour analyser et générer du JSON. Le module principal pour ces opérations est `json`, qui peut être facilement intégré dans n'importe quelle application Ruby.

### Analyser JSON :
Pour convertir une chaîne JSON en un hash Ruby, vous pouvez utiliser la méthode `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Sortie : {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Générer JSON :
Inversement, pour convertir un hash Ruby en une chaîne JSON, vous utilisez la méthode `JSON.generate` ou la méthode `to_json` disponible sur les objets Ruby une fois que la bibliothèque `json` est requise.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Sortie : {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Bibliothèques tierces :
Bien que la bibliothèque standard de Ruby couvre la gestion basique de JSON, de nombreux projets dépendent de bibliothèques tierces pour des fonctionnalités améliorées et des performances. Un choix populaire est `Oj` (Optimized JSON).

#### Analyser avec Oj :
```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Sortie : {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Générer avec Oj :
Oj offre également un moyen rapide de générer du JSON à partir d'objets Ruby :

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Sortie : {"name":"Samantha","age":35,"city":"Miami"}
```

Ces exemples illustrent la nature simple de travailler avec JSON en Ruby, le rendant accessible pour des tâches allant de simples manipulations de données à des communications d'API complexes.
