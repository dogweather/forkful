---
title:                "Mettre une chaîne en majuscules"
html_title:           "Elixir: Mettre une chaîne en majuscules"
simple_title:         "Mettre une chaîne en majuscules"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Elixir : Mettre en majuscule une chaîne de caractères

## Qu'est-ce que c'est et pourquoi ?

Mettre en majuscule une chaîne de caractères, c'est transformer la première lettre de chaque mot en majuscule. C'est utile pour une présentation propre du texte dans des scénarios comme les titres, les noms de personnes, etc.

## Voici comment :

Utilisez la fonction `capitalize/1` du module `String`.

```elixir
IO.puts(String.capitalize("elixir est super cool"))
```

Résultat :

```elixir
"Elixir est super cool"
```

## Plongée en profondeur

Historiquement, l'idée de mettre en majuscule une chaîne de caractères existe depuis longtemps en programmation. En Elixir, cela se fait avec la fonction `capitalize/1` qui fait partie du module `String`.

Il existe des alternatives, comme écrire votre propre fonction qui parcourt la chaîne et capitalise chaque mot. Cependant, l'utiliser sans nécessité est déconseillé car `String.capitalize/1` est plus performant et maintenu par la communauté Elixir.

La fonction `String.capitalize/1` applique le cas de titre aux valeurs de code point Unicode de la chaîne de caractères. Elle respecte l'Unicode et gère même correctement les lettres accentuées.

## Voir aussi

Pour plus d'informations sur les chaînes de caractères en Elixir, consultez les liens suivants :

- [Documentation officielle Elixir : Module String](https://hexdocs.pm/elixir/String.html)
- [Guide Elixir School sur les chaînes de caractères](https://elixirschool.com/fr/lessons/basics/strings/)