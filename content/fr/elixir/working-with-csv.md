---
title:                "Travailler avec les fichiers csv"
html_title:           "Elixir: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi travailler avec les fichiers CSV en utilisant Elixir ? Avec Elixir, vous pouvez traiter rapidement et efficacement des données structurées stockées dans des fichiers CSV. Cela peut être utile pour des tâches telles que la manipulation de données, l'analyse de données et l'importation/exportation de données.

## Comment faire

Pour travailler avec des fichiers CSV en utilisant Elixir, suivez ces étapes simples :

1. Tout d'abord, installez la librairie CSV en ajoutant `{:csv, "~> 2.0"}` à votre `mix.exs`.

2. Ensuite, importez la librairie CSV en appelant `require CSV` dans votre fichier.

3. Pour lire un fichier CSV, utilisez la fonction `CSV.parse/2` en passant le nom du fichier en tant que premier argument et les options de parsing en tant que deuxième argument. Par exemple :

```
Elixir iex> csv_data = CSV.parse("mon_fichier.csv", ["headers", "binary", "trim"])
```

4. Vous pouvez accéder aux données en utilisant la fonction `CSV.get_row/2` en passant l'index de la ligne et les données CSV en tant que deuxième argument. Par exemple :

```
Elixir iex> row = CSV.get_row(0, csv_data)
```

5. Pour écrire des données dans un nouveau fichier CSV, utilisez la fonction `CSV.encode/2` en passant les données à écrire et les options de parsing. Par exemple :

```
Elixir iex> new_csv_data = [%{"id" => "1", "name" => "John"}, %{"id" => "2", "name" => "Jane"}]
Elixir iex> CSV.encode(new_csv_data,["headers"])
```

## Plongée en profondeur

Voici quelques astuces supplémentaires pour travailler avec des fichiers CSV en utilisant Elixir :

- N'oubliez pas de toujours spécifier les options de parsing appropriées pour votre fichier CSV avec la fonction `CSV.parse/2`. Par exemple : `["headers", "binary", "trim"]`.
- Pour effectuer des opérations sur des colonnes spécifiques, utilisez la fonction `CSV.column/2` en passant le nom de la colonne et les données CSV en tant que deuxième argument.
- Si vous travaillez avec de grands fichiers CSV, il peut être plus efficace d'utiliser la fonction `CSV.stream!/2` pour traiter les données par lots, plutôt que de les charger toutes en mémoire en une seule fois.

## Voir aussi

Pour en savoir plus sur la manipulation de fichiers CSV en utilisant Elixir, consultez les ressources suivantes :

- [Documentation CSV](https://hexdocs.pm/csv/CSV.html)
- [Article sur le parsing CSV en Elixir](https://dev.to/rtfeldman/parsing-csv-in-elixir-please-dont-1386)