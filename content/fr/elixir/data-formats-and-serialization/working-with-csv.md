---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:05.397377-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ consiste \xE0 lire et \xE0 \xE9crire des donn\xE9es dans ces fichiers, un besoin\
  \ commun pour les\u2026"
lastmod: '2024-03-13T22:44:57.351719-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ consiste \xE0 lire et \xE0 \xE9crire des donn\xE9es dans ces fichiers, un besoin\
  \ commun pour les\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) consiste à lire et à écrire des données dans ces fichiers, un besoin commun pour les tâches nécessitant l'import/export de données ou des solutions de stockage simples. Les programmeurs exploitent cette fonctionnalité pour l'échange de données entre systèmes, la modification rapide de données, ou dans des situations où un format de données léger et facilement manipulable est avantageux.

## Comment faire :

Elixir, avec son puissant système de correspondance de motifs et son support pour le chaînage, peut gérer les fichiers CSV efficacement, même sans bibliothèques tierces. Cependant, pour des besoins plus avancés, la bibliothèque `nimble_csv` est un choix rapide et simple.

### Lire un fichier CSV Sans Bibliothèques Externes

Vous pouvez lire et analyser un fichier CSV en utilisant les fonctions intégrées d'Elixir :

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Exemple d'utilisation
CSVReader.read_file("data.csv")
# Sortie : [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### Écrire dans un fichier CSV

De manière similaire, pour écrire des données dans un fichier CSV :

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Exemple d'utilisation
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# Crée output.csv avec les données formatées en CSV
```

### Utiliser `nimble_csv`

Pour une manipulation plus complexe des CSV, `nimble_csv` offre une manière puissante et flexible de travailler avec les données CSV. Tout d'abord, ajoutez `nimble_csv` à vos dépendances dans `mix.exs` et exécutez `mix deps.get` :

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Analyser les données CSV avec `nimble_csv` :

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Exemple d'utilisation
MyCSVParser.parse("data.csv")
# La sortie avec nimble_csv peut être personnalisée selon la définition, mais elle ressemble généralement à une liste de listes ou de tuples, selon la configuration de votre analyseur.
```

Écrire des données CSV en utilisant `nimble_csv` nécessite de transformer manuellement vos données dans un format approprié puis de les écrire dans un fichier, un peu comme dans l'exemple d'Elixir simple, mais en exploitant `nimble_csv` pour générer des rangées CSV correctement formatées.

En choisissant l'approche appropriée en fonction de la complexité de votre tâche, vous pouvez manipuler les fichiers CSV en Elixir avec une grande flexibilité et puissance.
