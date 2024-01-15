---
title:                "Travailler avec json"
html_title:           "Ruby: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données dans un format lisible par les humains, comme une liste de contacts ou un tableau de données, vous pourriez avoir besoin de convertir ces données en un format lisible par les machines. C'est là qu'entre en jeu JSON.

## Comment faire

Pour utiliser JSON en Ruby, vous devez d'abord importer la bibliothèque standard "json". Ensuite, vous pouvez utiliser la méthode `JSON.parse()` pour convertir une chaîne en un objet Ruby et la méthode `JSON.generate()` pour convertir un objet Ruby en une chaîne JSON. Voici un exemple de code :

```Ruby
require "json"

# Convertir une chaîne JSON en un objet Ruby
data_string = '{"id":1, "name":"John", "age":25}'
data_object = JSON.parse(data_string)

# Convertir un objet Ruby en une chaîne JSON
person = {"id" => 2, "name" => "Jane", "age" => 28}
person_string = JSON.generate(person)

puts data_object["name"]
# Output: John

puts person_string
# Output: {"id":2, "name":"Jane", "age":28}
```

## Plongée en profondeur

JSON (JavaScript Object Notation) est un format de données léger et facile à comprendre pour les humains et les machines. Il est basé sur la syntaxe JavaScript et utilise des paires clé-valeur pour stocker les données. Les fichiers JSON ont généralement l'extension .json et sont couramment utilisés pour échanger des données entre différentes applications et services en ligne.

En utilisant la méthode `JSON.parse()`, vous pouvez également convertir des chaînes JSON en objets Ruby avec une structure de données complexe, y compris des tableaux et des objets imbriqués.

## Voir aussi

- ["Utilisez JSON avec Ruby"](https://www.rubyguides.com/2015/09/ruby-json-tutorial/)
- ["Manipuler des données JSON en Ruby"](https://stackify.com/json-ruby-beginners-guide/)
- ["Différences entre les formats JSON et XML"](https://www.sitepoint.com/javascript-json-xml-serialization/)