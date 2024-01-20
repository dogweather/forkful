---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'interpolation de chaînes en Elixir est une façon d'insérer et d'évaluer du code au sein d'une chaîne de caractères. Les programmeurs le font pour manipuler et formater dynamiquement les chaînes.

## Comment faire :

Dans Elixir, on utilise `#{}` pour interpoler une chaîne. Regardez ces exemples :

```Elixir
nom = "Jean"
IO.puts "Bonjour, #{nom}!"
```
Cela donnera en sortie :

```Elixir
Bonjour, Jean!
```
Par ailleurs, vous pouvez exécuter n'importe quelle expression Elixir à l'intérieur de l'interpolation. Par exemple :

```Elixir
IO.puts "Cinq plus cinq est #{5+5}."
```
Cela donnera en sortie :

```Elixir
Cinq plus cinq est 10.
```

## Un regard plus attentif 

1. Contexte historique : Elixir n'est pas le seul langage à avoir adopté l'interpolation de chaînes. De nombreux autres langages, notamment Ruby et Javascript, permettent également d'utiliser cette technique.

2. Alternatives : Si vous ne souhaitez pas utiliser l'interpolation, vous pouvez utiliser la concaténation, comme dans l'exemple ci-dessous :

    ```Elixir
    nom = "Jean"
    IO.puts("Bonjour, " <> nom <> "!")
    ```
   
3. Détails d'implémentation: Elixir effectue l'interpolation de chaîne en évaluant d'abord les expressions contenues dans `#{}`, avant de convertir le résultat en chaîne et de l'insérer dans la chaîne finale.

## À voir également 

Pour plus d'informations, vous pouvez consulter :

- La [documentation officielle Elixir](https://hexdocs.pm/elixir/String.html) sur les chaînes.
- [Elixir School: Interpolation](https://elixirschool.com/fr/lessons/basics/strings/), une explication simplifiée mais complète concernant l'interpolation de chaînes.
- [Elixir Forum](https://elixirforum.com/), un endroit pour discuter de toutes sortes de questions sur Elixir, y compris l'interpolation de chaînes.