---
title:                "Elixir: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation en Elixir, vous pourriez vous demander ce qu'est le JSON et pourquoi il est important pour votre code. JSON, ou JavaScript Object Notation, est un format de données largement utilisé pour échanger des informations entre les applications. En travaillant avec JSON, vous pouvez facilement transférer des données entre différents systèmes, ce qui est extrêmement utile dans les environnements de développement modernes.

## Comment faire

Voici quelques exemples de code dans lesquels vous pouvez utiliser JSON en utilisant Elixir:

```Elixir
# Créer un objet JSON
json = %{nom: "Jean", âge: 25, langue: "français"}

# Convertir en chaîne JSON
json_string = Jason.encode!(json)

# Analyser une chaîne JSON en un objet
parsed_json = Jason.decode!(json_string)

# Accéder aux valeurs spécifiques
IO.puts(parsed_json["nom"]) # Sortie: Jean
IO.puts(parsed_json["âge"]) # Sortie: 25 
```

Voici un autre exemple montrant comment écrire un objet JSON sur un fichier:

```Elixir
# Créer un objet JSON
json = %{ville: "Paris", population: 2_148_271, monuments: ["Tour Eiffel", "Arc de Triomphe", "Cathédrale Notre-Dame"]}

# Créer un fichier et écrire l'objet JSON dedans
File.write!("paris.json", Jason.encode!(json))
```

Et enfin, voici comment lire le contenu d'un fichier JSON et le convertir en un objet Elixir:

```Elixir
# Lire le fichier JSON
json_string = File.read!("paris.json")

# Convertir en objet Elixir
parsed_json = Jason.decode!(json_string)

# Accéder aux valeurs spécifiques
IO.puts(parsed_json["ville"]) # Sortie: Paris
IO.puts(parsed_json["population"]) # Sortie: 2_148_271
IO.puts(parsed_json["monuments"]) # Sortie: ["Tour Eiffel", "Arc de Triomphe", "Cathédrale Notre-Dame"]
```

## Plongée en profondeur

Maintenant que vous avez vu quelques exemples de base, voici un aperçu plus détaillé de quelques fonctionnalités liées au travail avec JSON en Elixir:

- Elixir a une bibliothèque standard appelée `Jason` qui facilite le travail avec JSON en fournissant des fonctions pour encoder et décoder des objets en JSON.
- Vous pouvez également utiliser `Poison`, une autre bibliothèque qui offre des fonctionnalités similaires à `Jason`.
- Les objets JSON en Elixir sont représentés sous forme de map, avec des paires clé-valeur.
- Elixir prend en charge différentes façons de représenter et de manipuler les données, ce qui en fait un langage idéal pour travailler avec des données JSON.

Maintenant que vous avez une meilleure compréhension de JSON et de son utilisation en Elixir, vous pouvez commencer à l'incorporer dans votre code pour échanger des données de manière efficace.

## Voir aussi

- [Documentation Elixir sur JSON](https://hexdocs.pm/elixir/1.11/elixir/Decode.html)
- [Elixir School - Parsing JSON with Elixir](https://elixirschool.com/fr/lessons/basics/basics-3/)
- [Jason GitHub repository](https://github.com/michalmuskala/jason)