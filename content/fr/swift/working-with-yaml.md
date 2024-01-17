---
title:                "Travailler avec yaml"
html_title:           "Swift: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que YAML et pourquoi les programmeurs l'utilisent-ils?

YAML, ou "YAML Ain't Markup Language", est un langage de sérialisation de données utilisé par les programmeurs pour stocker et transférer des données structurées de manière lisible pour l'homme. Il est souvent utilisé dans les projets de développement de logiciels pour stocker des configurations, des données de test et d'autres informations structurées.

## Comment faire :

```Swift
// Créer des données YAML :
let fruits = ["pomme", "banane", "orange"]
let yamlFruits = try YAMLEncoder().encode(fruits)
print(yamlFruits)

// Sortie :
"- pomme\n- banane\n- orange"
```

```Swift
// Décoder des données YAML :
let yamlCars = "- Chevrolet\n- Ford\n- Toyota"
let cars = try YAMLDecoder().decode([String].self, from: yamlCars)
print(cars)

// Sortie :
["Chevrolet", "Ford", "Toyota"]
```

## Plongée en profondeur :

Le format YAML a été créé en 2001 en réponse à d'autres formats de sérialisation tels que XML et JSON. Sa structure simple et lisible pour l'homme en font un choix populaire pour stocker des paramètres et des configurations dans les projets de développement de logiciels. D'autres formats de sérialisation courants incluent JSON, XML, et plist (pour les projets iOS et macOS).

## Voir aussi :

- La documentation officielle sur YAML : https://yaml.org/
- Une introduction complète à YAML : https://www.yamsuite.com/fr/blog/yaml-introduction
- Un outil en ligne pour tester du code YAML : https://yaml-online-parser.appspot.com/