---
title:                "Ruby: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi 

JSON, ou JavaScript Object Notation, est un format de données léger et facile à lire qui est largement utilisé dans le développement de logiciels. Il est principalement utilisé pour transmettre ou stocker des données structurées, telles que des données d'un serveur vers un client. En utilisant Ruby, on peut facilement manipuler et analyser des données JSON, ce qui peut être très utile dans de nombreuses applications.

## Comment faire

Pour travailler avec JSON en Ruby, nous allons tout d'abord utiliser la bibliothèque standard "json". Cette bibliothèque nous permet de convertir des objets Ruby en JSON et vice versa. Voici un exemple de code qui illustre comment convertir des données JSON en objets Ruby :

```Ruby
require 'json'

# Données JSON à convertir
data = '{"name": "Emma", "age": 25}'

# Convertir en objet Ruby
person = JSON.parse(data)

# Accéder aux valeurs des clés
puts person["name"] # Affiche Emma
puts person["age"] # Affiche 25
```
 
Nous pouvons également utiliser la méthode "to_json" pour convertir un objet Ruby en JSON :
 
```Ruby
person = {name: "Emma", age: 25}

# Convertir en JSON
json_data = person.to_json

# Afficher la donnée JSON
puts json_data # Affiche {"name": "Emma", "age": 25}
```

## Plongée en profondeur

En plus des méthodes de conversion, la bibliothèque "json" offre également d'autres fonctionnalités utiles. Par exemple, nous pouvons utiliser la méthode "pretty_generate" pour formater les données JSON de manière plus lisible pour les humains :

```Ruby
require 'json'

# Données JSON à formater
data = '[{"name": "Emma", "age": 25}, {"name": "John", "age": 30}]'

# Formater en JSON lisible
formatted_data = JSON.pretty_generate(JSON.parse(data))

# Afficher la donnée formatée
puts formatted_data
# Affiche :
# [
#   {
#     "name": "Emma",
#     "age": 25
#   },
#   {
#     "name": "John",
#     "age": 30
#   }
# ]
```

Il est également possible de valider si une donnée JSON est valide en utilisant la méthode "valid_json?" :

```Ruby
require 'json'

# Données JSON valide
data = '{"name": "Emma", "age": 25}'

# Vérifier la validité
puts JSON.valid_json?(data) # Affiche true
```

## Voir aussi

Pour plus d'informations sur la manipulation de données JSON en Ruby, voici quelques ressources utiles :

- [Documentation officielle de la bibliothèque standard "json"](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
- [Tutoriel vidéo : Travailler avec JSON en Ruby](https://www.youtube.com/watch?v=r6lFdVrMIxw)
- [Article de blog : Manipulation avancée de JSON en Ruby](https://www.freecodecamp.org/news/how-to-work-with-json-in-ruby-cope-with-advanced-apis-eed1d0c067c4/)