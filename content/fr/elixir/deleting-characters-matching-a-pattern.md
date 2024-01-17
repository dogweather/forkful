---
title:                "Suppression des caractères correspondants à un motif"
html_title:           "Elixir: Suppression des caractères correspondants à un motif"
simple_title:         "Suppression des caractères correspondants à un motif"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Supprimer des caractères correspondants à un motif est une tâche courante pour les programmeurs en Elixir. Cela implique de supprimer des caractères d'une chaîne de texte qui correspondent à un motif spécifié. Les programmeurs le font pour nettoyer et formater des données, ou pour filtrer des entrées utilisateur invalides.

## Comment faire:
Voici quelques exemples de code en Elixir pour supprimer des caractères correspondants à un motif :

```Elixir
# Supprimer les espaces dans une chaîne de texte
str = "Bonjour le monde!"
str |> String.replace!(" ", "")

# Supprimer toutes les voyelles dans une chaîne
str = "Bla bla bla"
str |> String.replace_re(~r/[aeiouy]/, "")

# Supprimer les nombres dans une chaîne
str = "123abc456"
str |> String.replace_re(~r/\d/, "")

# Supprimer les caractères spéciaux dans une chaîne
str = "@Hello, #world!"
str |> String.replace_re(~r/[^A-Za-z]/, "")
```

Résultats :

```
"Bonjourlemonde!" # Supprimer les espaces
"Bl bl bl" # Supprimer les voyelles
"abc" # Supprimer les nombres
"Helloworld" # Supprimer les caractères spéciaux
```

## Plongée en profondeur:
Supprimer des caractères correspondants à un motif n'est pas une tâche nouvelle en programmation. Elle est souvent utilisée pour nettoyer et préparer des données avant de les traiter davantage. Les expressions régulières sont une méthode couramment utilisée pour correspondre aux motifs dans une chaîne de texte. Il existe également d'autres méthodes de suppression de caractères, telles que l'utilisation de bibliothèques de manipulation de chaînes de texte.

## Voir aussi:
Pour en savoir plus sur la suppression de caractères correspondants à un motif en Elixir, consultez le [guide d'expression régulière Elixir](https://elixir-lang.org/getting-started/pattern-matching.html#regular-expressions) et la [documentation de la bibliothèque String Elixir](https://hexdocs.pm/elixir/String.html#content).