---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:33.693479-07:00
description: "Les expressions r\xE9guli\xE8res (regex) dans Elixir sont utilis\xE9\
  es pour chercher, faire correspondre, et manipuler des cha\xEEnes de caract\xE8\
  res bas\xE9es sur des\u2026"
lastmod: '2024-03-13T22:44:57.316305-06:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) dans Elixir sont utilis\xE9es pour\
  \ chercher, faire correspondre, et manipuler des cha\xEEnes de caract\xE8res bas\xE9\
  es sur des\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les expressions régulières (regex) dans Elixir sont utilisées pour chercher, faire correspondre, et manipuler des chaînes de caractères basées sur des modèles spécifiques. Les programmeurs utilisent les regex pour des tâches telles que la validation de formats (email, URLs), l'analyse de journaux, ou l'extraction de données, grâce à leur efficacité et versatilité dans la manipulation de chaînes.

## Comment faire :

Elixir utilise le module `Regex`, qui s'appuie sur la bibliothèque regex d'Erlang, pour les opérations regex. Voici des utilisations de base :

```elixir
# Faire correspondre un modèle - Renvoie la première correspondance
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Sortie : ["hello"]

# Trouver toutes les correspondances
all_matches = Regex.scan(~r/\d/, "Il y a 2 pommes et 5 oranges.")
IO.inspect(all_matches) # Sortie : [["2"], ["5"]]

# Remplacer des parties d'une chaîne
replaced_string = Regex.replace(~r/\s+/, "Elixir est amusant", "_")
IO.inspect(replaced_string) # Sortie : "Elixir_est_amusant"
```

Pour des modèles plus complexes et des fonctionnalités, vous pourriez envisager d'utiliser des bibliothèques tierces, bien que pour la plupart des tâches de correspondance de chaînes et de motifs, le module `Regex` intégré d'Elixir est assez puissant.

Pour réaliser une correspondance insensible à la casse, utilisez l'option `i` :

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Sortie : ["Hello"]
```

Les expressions regex peuvent être précompilées pour l'efficacité lorsqu'elles sont utilisées plusieurs fois :

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Sortie : ["hello"]
```

Elixir prend également en charge les captures nommées, qui peuvent être très pratiques pour extraire des parties spécifiques d'une chaîne tout en rendant votre code plus lisible :

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Sortie : %{"year" => "2023", "month" => "04", "day" => "15"}
```

Cette brève vue d'ensemble souligne la facilité avec laquelle Elixir gère les expressions régulières, permettant des techniques puissantes de manipulation de chaînes et d'extraction de données.
