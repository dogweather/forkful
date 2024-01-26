---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:36.139678-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Concaténer des chaînes c'est les joindre bout à bout. C'est utile pour assembler des textes, des chemins de fichiers, ou pour créer des messages.

## Comment faire :
```elixir
# Concaténation simple avec <>
prenom = "Jean"
nom = "Dupont"
nom_complet = prenom <> " " <> nom
IO.puts nom_complet # Affiche: Jean Dupont

# Utilisation de `String.concat/2`
message = String.concat("Bonjour, ", nom_complet)
IO.puts message # Affiche: Bonjour, Jean Dupont

# Assembler une liste de chaînes avec `Enum.join/2`
liste_mots = ["Salut", "monde", "!"]
phrase = Enum.join(liste_mots, " ")
IO.puts phrase # Affiche: Salut monde !
```

## Exploration approfondie
Historiquement, concaténer des chaînes était une opération coûteuse en termes de performances. En Elixir, les chaînes sont des binaires UTF-8, et utiliser `<>` est efficace car Elixir fait des optimisations sous le capot. Les alternatives incluent l'utilisation de fonctions comme `String.concat/2` ou `Enum.join/2`, selon le contexte. `Enum.join/2` est idéal pour les listes de mots alors que `String.concat/2` se prête mieux pour unir deux chaînes simplement.

## Voir aussi
- Documentation Elixir sur les chaînes : https://hexdocs.pm/elixir/String.html
- Tutoriel Elixir School sur les chaînes (en anglais) : https://elixirschool.com/en/lessons/basics/strings/
