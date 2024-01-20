---
title:                "Travailler avec json"
html_title:           "Gleam: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-json.md"
---

{{< edit_this_page >}}

JSON: Une Introduction pour les Programmateurs

## Qu'est-ce que c'est et pourquoi?

JSON, ou JavaScript Object Notation, est un format de données largement utilisé pour échanger des informations entre différentes applications. En tant que programmeur, vous utiliserez probablement JSON pour transmettre et stocker des données dans un format standardisé. JSON est également un choix populaire pour les API en raison de sa simplicité et de sa légèreté.

## Comment faire:

Voici quelques exemples simples de création et de manipulation de JSON en utilisant Gleam:

```Gleam
// Création d'un objet JSON avec une clé et une valeur
let data = %{"nom": "Marie"} 

// Accédez à la valeur de la clé
let nom = data["nom"]

// Ajoutez une nouvelle clé et valeur à l'objet JSON existant
data = data |> Map.put("âge", 28)

// Convertissez un object Gleam en JSON
let json_data = Json.Encode.encode(data)
```

Voici un exemple de sortie JSON:

```json
{
  "nom": "Marie",
  "âge": 28
}
```

## Plongée en profondeur:

Les origines de JSON remontent à JavaScript, où il a été conçu comme un format de données simple pour les applications web. Actuellement, JSON est pris en charge par de nombreuses langues de programmation et est devenu un standard pour le partage de données.

Bien que JSON soit populaire, il existe des alternatives telles que XML et YAML. L'avantage de JSON est sa clarté, sa simplicité et sa lisibilité pour les humains. Il est également facilement analysé par les ordinateurs.

En utilisant Gleam, vous pouvez facilement créer des objets JSON à l'aide de la bibliothèque standard Json.Encode. Vous pouvez également utiliser Json.Decode pour analyser des données JSON et les convertir en objets Gleam pour une manipulation facile.