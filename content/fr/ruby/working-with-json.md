---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec JSON (JavaScript Object Notation) permet d'échanger des données facilement entre applications. C'est essentiel pour le web, les APIs, et les applis modernes pour la simplicité et la portabilité.

## How to:
```Ruby
require 'json'

# Créer un hash et le convertir en JSON
user_info = { name: "Jean", age: 30, city: "Paris" }
user_json = user_info.to_json
puts user_json # {"name":"Jean","age":30,"city":"Paris"}

# Parser une string JSON pour obtenir un hash Ruby
parsed_info = JSON.parse(user_json)
puts parsed_info["city"] # Paris
```

## Deep Dive
JSON est né en 2001, conçu par Douglas Crockford. Il devient vite populaire pour sa simplicité par rapport à XML. Ruby fournit une librairie standard `json` pour le traitement. YAML et BSON sont des alternatives, chacun a ses cas d’usage.

## See Also
- [JSON.org](https://www.json.org/json-fr.html) pour les détails sur le format JSON.
- [O'Reilly's "JSON: A Beginners Guide"](https://www.oreilly.com/library/view/json-a-beginners/9780470526910/) pour plus d'apprentissage sur JSON.
