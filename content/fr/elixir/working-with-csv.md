---
title:                "Travailler avec des fichiers csv"
html_title:           "Elixir: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi et pour quoi?

Travailler avec des fichiers CSV est une tâche courante pour les programmeurs. CSV, ou Comma-Separated Values, est un format de fichier qui stocke des données tabulaires sous forme de valeurs séparées par des virgules. Les programmeurs utilisent des fichiers CSV pour stocker des données qui peuvent être facilement lues et modifiées par des utilisateurs humains et par des programmes.

## Comment faire?

Pour travailler avec des fichiers CSV en Elixir, il existe une excellente bibliothèque appelée CSV. Tout d'abord, nous devons l'ajouter à notre projet en utilisant la commande `mix deps.get` et en ajoutant `{:csv, "~> 2.1"}` à notre fichier `mix.exs`. Ensuite, nous pouvons utiliser le module CSV pour lire et écrire les données.

```Elixir
# Lecture de données CSV
"C:/Users/User/Desktop/data.csv"
|> CSV.parse!(headers: true)
|> Enum.each(fn row -> IO.inspect row end) # affiche chaque ligne du fichier

# Ecriture de données CSV
[
  ["Nom", "Âge", "Ville"],
  ["Alice", "25", "Paris"],
  ["Bob", "30", "Lyon"]
]
|> CSV.encode
|> File.write("C:/Users/User/Desktop/data1.csv") # écrit le fichier CSV
```

## Plongée en profondeur

Les fichiers CSV ont été créés à l'origine pour stocker des données dans des feuilles de calcul et sont maintenant largement utilisés pour échanger des données entre différents systèmes. En dehors de la bibliothèque CSV, il existe d'autres alternatives telles que FasterCSV et CSVix qui offrent des fonctionnalités supplémentaires telles que la gestion des caractères spéciaux et des gros fichiers.

L'implémentation de CSV en Elixir utilise les protocoles Enumerable et Collectable, ce qui permet une manipulation efficace des données. De plus, il est possible d'étendre les fonctionnalités de CSV en implémentant ces protocoles pour d'autres formats de données.

## Voir aussi

- [Documentation de la bibliothèque CSV](https://hexdocs.pm/csv/2.1.0/index.html)
- [FasterCSV](https://hex.pm/packages/faster_csv)
- [CSVix](https://hex.pm/packages/csvix)