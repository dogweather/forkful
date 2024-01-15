---
title:                "Convertir une chaîne de caractères en minuscules"
html_title:           "Elixir: Convertir une chaîne de caractères en minuscules"
simple_title:         "Convertir une chaîne de caractères en minuscules"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

De nos jours, la manipulation de chaînes de caractères est un élément essentiel dans le développement de logiciels. Que ce soit pour traiter des données utilisateur, créer des rapports ou simplement formater du texte, il est souvent nécessaire de convertir une chaîne de caractères en minuscules. Dans cet article, nous allons explorer comment faire cela en utilisant Elixir, un langage de programmation fonctionnelle et dynamique.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant Elixir, nous pouvons utiliser la fonction `String.downcase/1`. Elle prend une chaîne de caractères en entrée et renvoie une nouvelle chaîne de caractères avec tous les caractères en minuscules. Voici un exemple:

```elixir
iex> String.downcase("BONJOUR")
"bonjour"
```

Nous pouvons également utiliser la syntaxe de "pipe" pour rendre le code plus lisible et éviter les répétitions inutiles:

```elixir
iex> "BONJOUR" |> String.downcase()
"bonjour"
```

## Plongée approfondie

Il est important de noter que la fonction `String.downcase/1` utilise l'encodage Unicode pour la conversion en minuscules. Cela signifie que les caractères accentués et spéciaux seront également convertis en minuscules selon les règles de l'Unicode. Par exemple:

```elixir
iex> "ÉLÉPHANT" |> String.downcase()
"éléphant"
```

De plus, si vous avez besoin de convertir une seule lettre en minuscule, vous pouvez utiliser la fonction `String.downcase/2` en spécifiant l'indice de la lettre à convertir. Voici un exemple:

```elixir
iex> "Bonjour" |> String.downcase(3)
"bonjour"
```

Maintenant que vous savez comment convertir une chaîne de caractères en minuscules en utilisant Elixir, vous pouvez facilement l'appliquer à vos projets pour améliorer la lisibilité et la manipulation des données.

## Voir aussi

- [Documentation officielle Elixir pour String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Tutoriel de base Elixir pour devenir un expert en chaînes de caractères](https://elixirschool.com/fr/lessons/basics/strings/)