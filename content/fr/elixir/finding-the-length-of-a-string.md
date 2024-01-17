---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Elixir: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Trouver la longueur d'une chaîne de caractères est une opération courante en programmation. Cela implique de déterminer le nombre de caractères présents dans une chaîne donnée. Les programmeurs doivent souvent trouver la longueur d'une chaîne pour effectuer différentes manipulations de données, telles que la concaténation ou la comparaison de chaînes.

## Comment faire:

Voici comment vous pouvez trouver la longueur d'une chaîne de caractères en utilisant Elixir:

```
Elixir 
str = "Bonjour le monde"
IO.puts(String.length(str))
``` 
Output:
```
16
```

Dans cet exemple, nous avons utilisé la fonction `String.length` pour obtenir la longueur de la chaîne "Bonjour le monde". Cette fonction renvoie un entier représentant le nombre de caractères dans la chaîne donnée.

## Analyse Approfondie:

La nécessité de trouver la longueur d'une chaîne de caractères remonte aux premiers jours de la programmation informatique. Cela était dû au fait que les ordinateurs stockaient les chaînes de caractères dans un emplacement de mémoire prédéterminé avec une taille fixe. Aujourd'hui, avec les langages de haut niveau comme Elixir, la longueur des chaînes peut varier et être gérée dynamiquement.

En plus d'utiliser la fonction `String.length`, il existe d'autres façons de trouver la longueur d'une chaîne en Elixir. Par exemple, vous pouvez utiliser le module Enum et sa fonction `count` pour compter le nombre de caractères dans une chaîne :

```
Elixir
str = "Bonjour le monde"
IO.puts(Enum.count(str))
```

De manière similaire, vous pouvez utiliser la fonction `length` du module Enum pour obtenir la longueur d'une liste de caractères :

```
Elixir
str = ["Bonjour", "le", "monde"]
IO.puts(Enum.length(str))
```

En termes d'implémentation, la fonction `String.length` utilise la fonction interne `String.length/1`, tandis que la fonction `Enum.count` utilise la fonction interne `Enum.reduce/3`.

## Voir aussi:

Pour plus d'informations sur les fonctions `String.length` et `Enum.count` en Elixir, consultez la documentation officielle : https://hexdocs.pm/elixir/String.html#length/1 et https://hexdocs.pm/elixir/Enum.html#count/1.

Vous pouvez également découvrir d'autres méthodes pour trouver la longueur d'une chaîne de caractères en Elixir en consultant ce lien : https://www.tutorialspoint.com/elixir/elixir_data_types.htm.