---
title:                "Elixir: Assembler des chaînes de caractères"
simple_title:         "Assembler des chaînes de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une tâche courante en programmation et peut être utile dans de nombreux cas, tels que la création de messages personnalisés, la manipulation de données ou la construction de requêtes HTTP. Cela permet également de rendre votre code plus lisible en évitant les lignes excessivement longues.

## Comment

Voici quelques exemples de code en Elixir pour concaténer des chaînes de caractères :

```Elixir
"Bonjour" <> " " <> "tout le monde" #=> "Bonjour tout le monde"
"J'ai " <> "2" <> " pommes" #=> "J'ai 2 pommes"
[1, 2, 3] |> Enum.join("-") #=> "1-2-3"
```

Les opérateurs `<>` et `|>` peuvent être utilisés pour concaténer des chaînes de caractères de manière simple et efficace. Il est également possible d'utiliser la fonction `String.concat/2` en passant une liste de chaînes de caractères à concaténer.

```Elixir
String.concat(["Bonjour", " ", "tout le monde"]) #=> "Bonjour tout le monde"
```

## Plongée en profondeur

Lorsque vous concaténez des chaînes de caractères, gardez à l'esprit que cela peut être une opération coûteuse en termes de performances, surtout si vous concaténez de grandes quantités de données. En effet, à chaque fois que vous concaténez une chaîne, une nouvelle chaîne est créée en mémoire, ce qui peut être gênant si vous répétez l'opération plusieurs fois.

Pour éviter cela, il est recommandé d'utiliser des listes de chaînes de caractères plutôt que des chaînes à concaténer. Ensuite, vous pouvez utiliser la fonction `Enum.join/2` pour les fusionner en une seule chaîne. Par exemple :

```Elixir
"My " <> "real" <> " " <> "name" #=> "My real name"
["My", "real", "name"] |> Enum.join(" ") #=> "My real name"
```

## Voir aussi

- [Documentation Elixir sur la concaténation de chaînes](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#string-interpolation)
- [Article sur les bonnes pratiques de performance en Elixir](https://codeburst.io/performance-tips-in-elixir-6f52b748ff35)
- [Vidéo explicative sur la concaténation en Elixir](https://www.youtube.com/watch?v=I7Yd2ROufQo&t=304s)