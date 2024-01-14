---
title:    "Elixir: Capitalisation d'une chaîne de caractères"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être remarqué que certains mots dans votre code Elixir sont écrits en majuscules, notamment les noms de fonctions et de modules. Mais pourquoi est-il important de suivre cette convention de nommage ? La réponse est simple : il s'agit d'une convention largement utilisée dans la communauté Elixir pour améliorer la lisibilité du code et faciliter la compréhension. 

## Comment faire

En Elixir, il existe plusieurs façons de capitaliser une chaîne de caractères. Voici un exemple de code utilisant la fonction `String.capitalize` pour mettre la première lettre d'une chaîne en majuscule : 

```Elixir
string = "elixir"
String.capitalize(string) # renvoie "Elixir"
```

On peut également utiliser la fonction `String.upcase` pour mettre toutes les lettres d'une chaîne en majuscule : 

```Elixir
string = "elixir"
String.upcase(string) # renvoie "ELIXIR"
```

Pour mettre en majuscule la première lettre de chaque mot d'une chaîne, on peut utiliser la fonction `String.capitalize_words` : 

```Elixir
string = "elixir programming"
String.capitalize_words(string) # renvoie "Elixir Programming"
```

## Plongée dans les détails

La raison pour laquelle ces fonctions sont très utiles est qu'elles nous permettent de garder notre code cohérent et facile à lire. La convention de nommage en Elixir suit généralement cette règle : les noms de fonctions et de modules sont écrits en camelCase, ce qui signifie que la première lettre de chaque mot est en majuscule sauf pour le premier mot. De plus, en Elixir, les chaînes de caractères sont immuables, c'est-à-dire qu'elles ne peuvent pas être modifiées directement. Cela signifie que les fonctions `String.capitalize` et `String.upcase` renvoient une nouvelle chaîne de caractères avec la modification apportée. 

## Voir aussi

- [Documentation officielle d'Elixir pour la manipulation de chaînes de caractères](https://elixir-lang.org/getting-started/string.html)
- [Blog post sur les conventions de nommage en Elixir](https://medium.com/the-road-to-elixir/naming-conventions-in-elixir-5eec5988468a)