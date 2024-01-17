---
title:                "Mettre en majuscules une chaîne de caractères."
html_title:           "Elixir: Mettre en majuscules une chaîne de caractères."
simple_title:         "Mettre en majuscules une chaîne de caractères."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Capitaliser une chaîne de caractères est simplement le fait de mettre la première lettre de chaque mot en majuscule, tout en laissant les autres lettres en minuscule. Les programmeurs font cela pour la lisibilité et la cohérence du code. 

## Comment faire:

```
Elixir
iex> String.capitalize("hello world")
"Hello world"
```

Dans cet exemple, nous utilisons la fonction `capitalize` du module `String` pour capitaliser la chaîne "hello world". Le résultat renvoie "Hello world".

```
Elixir
iex> String.capitalize("élève")
"Élève"
```

Dans ce deuxième exemple, nous pouvons voir que même avec des caractères accentués, la fonction `capitalize` fonctionne correctement et renvoie le bon résultat.

## Plongée en profondeur:

Historiquement, le concept de capitalisation est issu du domaine de la typographie, où les premières lettres des phrases étaient écrites en majuscules pour une meilleure lisibilité. Dans le monde de la programmation, cela est également important pour distinguer les noms de variables, de fonctions, etc. des mots clés du langage.

Une alternative à la fonction `capitalize` de Elixir serait d'utiliser la fonction `titlecase` qui met en majuscule la première lettre de chaque mot, mais qui convertit également les caractères suivants en minuscules.

Le principe derrière la capitalisation d'une chaîne de caractères est en fait assez simple. En utilisant des règles de grammaire et des expressions régulières, le langage peut déterminer quelles lettres doivent être mises en majuscule ou en minuscule.

## Voir aussi:

- [Documentation sur les chaînes de caractères en Elixir](https://hexdocs.pm/elixir/String.html)