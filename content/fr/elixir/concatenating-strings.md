---
title:    "Elixir: Chaînage de chaînes de caractères"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une opération courante dans la programmation, surtout dans les langages de script comme Elixir. Cette opération permet de combiner plusieurs chaînes de caractères pour former une seule chaîne plus longue. Cela peut être utile lors de la création de messages à afficher à l'utilisateur, ou pour la construction d'URLs dynamiques.

## Comment faire

Pour concaténer des chaînes de caractères en Elixir, on utilise l'opérateur `<>` comme ceci :

```elixir
"Hello" <> " world"           # output: "Hello world"
"5" <> " + " <> "5"           # output: "5 + 5"
"Il fait " <> 25 <> " degrés" # output: "Il fait 25 degrés"
```

On peut également utiliser la fonction `++` pour concaténer plusieurs chaînes en une seule :

```elixir
["J'aime", " ", "le", " ", "programmation"] ++ [" en Elixir"]  # output: "J'aime le programmation en Elixir"
```

Pour plus de flexibilité, on peut utiliser la fonction `Enum.join/2` qui nous permet de spécifier un délimiteur entre chaque élément à concaténer :

```elixir
["Alice", "Bob", "Carol"] |> Enum.join("-")          # output: "Alice-Bob-Carol"
["1", "2", "3"] |> Enum.join(", ") |> Enum.join(" ") # output: "1, 2, 3"
```

## Plongée en profondeur

Derrière le concept simple de la concaténation de chaînes de caractères se cache en réalité une opération plus complexe. Lorsqu'on utilise l'opérateur `<>` ou la fonction `++`, Elixir crée en fait une nouvelle chaîne en copiant les données de chaque chaîne et en les plaçant dans une nouvelle zone mémoire.

Cela signifie que si nous concaténons plusieurs fois des chaînes de caractères, cela peut rapidement devenir inefficace en terme de performance. C'est pourquoi, lorsqu'il s'agit de concaténer un grand nombre de chaînes, il est préférable d'utiliser la fonction `Enum.join/2` qui utilise une méthode plus efficace en termes de mémoire et de temps d'exécution.

## Voir aussi

- La documentation officielle d'Elixir sur la concaténation de chaînes de caractères : https://hexdocs.pm/elixir/String.html#module-join
- Un article sur les meilleures pratiques en matière de performances en Elixir : https://evilmartians.com/chronicles/elixir-best-practices-performance-and-responsiveness
- Un tutoriel sur la manipulation de chaînes de caractères en Elixir : https://pragmaticstudio.com/tutorials/elixir-strings