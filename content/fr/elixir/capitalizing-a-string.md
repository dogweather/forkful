---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Elixir: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

La capitalisation de chaînes de caractères est une tâche courante en programmation, que ce soit pour améliorer la lisibilité des données ou pour de la manipulation de texte. Avec Elixir, vous pouvez facilement réaliser cette opération grâce à des fonctions intégrées dans le langage.

## Comment faire

Pour capitaliser une chaîne de caractères en Elixir, vous pouvez utiliser la fonction `String.capitalize/1`. Cette fonction prend en paramètre la chaîne de caractères que vous souhaitez capitaliser et renvoie une nouvelle chaîne avec la première lettre en majuscule.

```Elixir 
iex> String.capitalize("bonjour")
"Bonjour"
```

Il est également possible de capitaliser toutes les lettres d'une chaîne de caractères avec la fonction `String.upcase/1`.

```Elixir
iex> String.upcase("bonjour")
"BONJOUR"
```

Si vous souhaitez seulement capitaliser la première lettre d'une phrase, vous pouvez utiliser la fonction `String.capitalize_words/1`.

```Elixir
iex> String.capitalize_words("bonjour tout le monde")
"Bonjour Tout Le Monde"
```

## Plongée en profondeur

Derrière ces fonctions pratiques se cache en réalité une utilisation de la librairie standard d'Elixir, `String`, qui fournit de nombreuses autres fonctions utiles pour la manipulation de chaînes de caractères.

Il est bon de noter que la plupart des fonctions de manipulation de chaînes en Elixir renvoient une nouvelle chaîne plutôt que de modifier la chaîne d'origine. Cela est dû au fait que les chaînes de caractères sont immuables en Elixir, c'est-à-dire qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées. Cela permet à Elixir de garantir une immutabilité complète et de mieux gérer l'état de vos données.

# Voir également

- [Documentation officielle d'Elixir sur les chaînes de caractères](https://hexdocs.pm/elixir/String.html)
- [Blog post sur la manipulation de chaînes de caractères en Elixir](https://medium.com/@nicoamador99/elixir-strings-manipulation-made-easy-2c3b2ffde7df)