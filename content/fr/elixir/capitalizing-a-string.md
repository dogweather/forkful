---
title:                "Elixir: Capitaliser une chaîne"
simple_title:         "Capitaliser une chaîne"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de capitaliser une chaîne de caractères dans un programme Elixir, que ce soit pour des raisons d'affichage esthétique ou pour des besoins de manipulation de données. Dans cet article, nous allons explorer comment effectuer cette opération de manière efficace et élégante.

## Comment Faire

Pour capitaliser une chaîne de caractères en Elixir, il existe plusieurs options selon vos besoins spécifiques. Voici quelques exemples :

```Elixir
string = "bonjour le monde"

# En utilisant la fonction String.capitalize/1
capitalized = String.capitalize(string)
IO.puts capitalized
# Output: Bonjour le monde

# En utilisant la fonction String.replace/3 et une expression régulière
capitalized = String.replace(string, ~r/.*/, &String.capitalize/1)
IO.puts capitalized
# Output: Bonjour le monde

# En utilisant une méthode de chaîne de caractères en mutable
capitalized = string |> String.to_charlist() |> :lists.map(fn(x) -> String.capitalize(x) end) |> to_string()
IO.puts capitalized
# Output: Bonjour le monde
```

Comme vous pouvez le voir, il existe plusieurs façons d'accomplir cette tâche en Elixir, chacune avec ses avantages et ses limites. À vous de choisir celle qui convient le mieux à votre situation.

## Deep Dive

Maintenant, rentrons un peu plus dans les détails et examinons comment fonctionne la fonction String.capitalize/1. Elle prend en paramètre une chaîne de caractères et renvoie cette même chaîne de caractères avec la première lettre en majuscule. Facile, n'est-ce pas ?

Mais attention, cette fonction utilise la convention Unicode pour les majuscules et les minuscules, ce qui signifie que les caractères accentués ne seront pas forcément convertis comme vous le souhaitez. Par exemple, "éclat" deviendra "Éclat" et non pas "ÉClat" comme on pourrait s'y attendre.

Si vous souhaitez conserver la casse exacte du reste de la chaîne et ne modifier que la première lettre, alors String.replace/3 avec une expression régulière sera votre meilleur allié. En utilisant `[a-z]` comme motif dans l'expression régulière, cela signifie "tous les caractères de a à z", et ainsi, seuls les caractères minuscules seront touchés.

## Voir Aussi

- [Documentation officielle de la fonction String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Tutoriel sur les expressions régulières en Elixir](https://elixir-lang.org/getting-started/regex.html)
- [Article sur les méthodes de chaîne de caractères en mutable en Elixir](https://www.poeticoding.com/elixir-strings-functions-to-char-list-from-char-list-to-string/)