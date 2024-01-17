---
title:                "Travailler avec JSON"
html_title:           "Ruby: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# Quoi et Pourquoi?
JSON, ou JavaScript Object Notation, est un format standard pour échanger des données entre applications. Les programmeurs utilisent JSON car il est facile à lire et à écrire pour les humains et à traiter pour les ordinateurs. Il est également léger et largement pris en charge par les langages de programmation.

# Comment faire:
Voici un exemple de code Ruby pour convertir un objet en JSON et vice versa:

```Ruby
require 'json'

# convertir un objet en JSON
obj = { nom: "Jean", age: 25 }
json_string = obj.to_json

# convertir du JSON en objet
obj = JSON.parse(json_string)

puts obj[:age] # affiche 25
```

# Plongée dans le sujet:
JSON a été créé en 2001 par Douglas Crockford pour remplacer XML en tant que format léger pour échanger des données. Bien que JSON soit principalement utilisé avec JavaScript, il est largement pris en charge par d'autres langages de programmation tels que Ruby, Python et Java. Il existe également des alternatives à JSON telles que YAML et CSV pour stocker et échanger des données structurées.

# Voir aussi:
- Documentation officielle de la gem Ruby JSON: https://github.com/flori/json
- Site web officiel de JSON: https://www.json.org/