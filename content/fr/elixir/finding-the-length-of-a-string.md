---
title:                "Elixir: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a souvent des tâches simples en programmation qui peuvent sembler sans importance, mais qui peuvent avoir de grandes conséquences sur la qualité de notre code. Trouver la longueur d'une chaîne de caractères peut sembler être une tâche banale, mais cela peut nous aider à éviter des erreurs critiques dans notre code.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Elixir, nous pouvons utiliser la fonction `String.length/1`. Cette fonction prend en paramètre une chaîne de caractères et renvoie le nombre total de caractères dans cette chaîne. Voyons un exemple concret :

```Elixir
string = "Bonjour"
String.length(string)
```
La sortie de ce code sera `7`, car il y a 7 caractères dans la chaîne "Bonjour".

Un autre moyen de trouver la longueur d'une chaîne de caractères est d'utiliser l'opérateur `|>`. Cet opérateur, appelé "pipe", prend le résultat d'une première opération et l'injecte comme premier argument d'une seconde opération. Dans notre cas, nous pouvons utiliser cet opérateur avec la fonction `String.length/1` de cette manière :

```Elixir
string = "Bonjour"
string |> String.length()
```

La sortie sera la même que précédemment, `7`. N'oubliez pas que l'opérateur `|>` fonctionne de gauche à droite, donc nous pouvons également écrire `String.length(string) |> String.capitalize()` pour trouver la longueur de la chaîne et mettre la première lettre en majuscule.

## Plongée en profondeur

Maintenant que nous savons comment trouver la longueur d'une chaîne en Elixir, il est important de comprendre comment cela fonctionne réellement en coulisses. En réalité, la fonction `String.length/1` utilise une méthode optimisée pour accéder à la mémoire sous-jacente de la chaîne et renvoyer sa longueur. Cela signifie que cette fonction est très rapide et efficace, même pour les chaînes de caractères très longues.

De plus, la fonction `String.length/1` prend en compte la longueur réelle de la chaîne, et non seulement le nombre de caractères visibles. Cela signifie qu'elle est également utile pour trouver la longueur des chaînes contenant des caractères spéciaux ou des emoji.

## Voir aussi

Voici quelques liens supplémentaires pour en savoir plus sur la manipulation des chaînes de caractères en Elixir :

- [La documentation officielle d'Elixir sur la manipulation des chaînes de caractères](https://elixir-lang.org/getting-started/string.html)
- [Un article sur les opérateurs de pipe en Elixir](https://www.tutos.eu/blog/elixir-les-operateurs-pipe.html)
- [Un cours sur les bases de la programmation en Elixir, y compris la manipulation de chaînes de caractères](https://openclassrooms.com/fr/courses/1894236-programmez-en-elixir/1894381-mes-premiers-pas-avec-elixir)