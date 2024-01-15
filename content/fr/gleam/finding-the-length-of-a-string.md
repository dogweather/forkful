---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Gleam: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi
Avouons-le, tout le monde a déjà eu besoin de connaître la longueur d'une chaîne de caractères dans un programme. Que ce soit pour valider la saisie d'un utilisateur ou pour manipuler des données, cette opération est incontournable. Heureusement, en utilisant Gleam, trouver la longueur d'une chaîne de caractères est un jeu d'enfant !

## Comment faire
Pour trouver la longueur d'une chaîne de caractères en utilisant Gleam, vous pouvez utiliser la fonction `length()` en passant en argument votre chaîne de caractères. Voici un exemple de code avec une chaîne de caractères prédéfinie :
```Gleam
let string = "Bonjour le monde !"
let length = string |> length()
```
Ce code va renvoyer la valeur `18` dans la variable `length` car il y a 18 caractères dans la chaîne "Bonjour le monde !". 
Il est également possible de trouver la longueur d'une chaîne de caractères en utilisant la méthode `String.len()` :
```Gleam
let string = "Hello"
let length = String.len(string)
```
Ce code renvoie également la valeur `5` dans la variable `length`.

## Plongée en profondeur
En cherchant à trouver la longueur d'une chaîne de caractères en utilisant Gleam, il est important de comprendre que la fonction `length()` et la méthode `String.len()` font la même chose en interne. La fonction `length()` est simplement un raccourci pour la méthode `String.len()`. Vous pouvez donc utiliser l'une ou l'autre selon vos préférences. 
Une autre chose à noter est que ces méthodes ne fonctionnent qu'avec les chaînes de caractères et non avec d'autres types de données. Si vous essayez de trouver la longueur d'un entier ou d'un booléen, par exemple, cela entraînera une erreur.

# Voir aussi
Pour en savoir plus sur les différentes fonctions et méthodes disponibles en Gleam, vous pouvez consulter la documentation officielle sur les chaînes de caractères :
- [Documentation Gleam sur les chaînes de caractères](https://gleam.run/core/string.html)
- [Documentation Gleam sur les fonctions prédéfinies](https://gleam.run/core/functions.html)
- [Documentation Gleam sur les opérateurs](https://gleam.run/core/operators.html)