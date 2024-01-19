---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Trouver la longueur d'une chaîne consiste à déterminer le nombre de caractères qu'elle contient. Les programmeurs le font pour gérer précisément la manipulation et le traitement des données textuelles.

## Comment faire:
Voici comment vous pouvez trouver la longueur d'une chaîne en Gleam:

```gleam
import gleam/string.{length}
let ma_chaine = "Programmation Gleam"
let longueur = length(ma_chaine)
```

Lors de l'exécution de ce code, la variable `longueur` obtiendra une valeur de `19`, qui est le nombre de caractères dans "Programmation Gleam".

## Plongée Profonde
Historiquement, l'opération de calcul de la longueur d'une chaîne est une nécessité fondamentale dans la programmation. Elle permet de faire de nombreuses choses comme le découpage de chaînes, la vérification des saisies utilisateur, et bien plus encore.

En Gleam, une alternative pour trouver la longueur d'une chaîne serait d'implémenter manuellement une fonction qui utilise une boucle pour compter les caractères. Cependant, l'utilisation de la fonction intégrée `length` est beaucoup plus efficace et rapide.

### Détails de mise en œuvre
En interne, Gleam stocke les chaînes en tant que liste de caractères. La fonction `length` compte simplement ces caractères. C'est une opération O(1), ce qui signifie qu'elle s'exécute en temps constant, peu importe la longueur de la chaîne.

## Aussi Voir:
1. Documentation officielle de Gleam: <a href='https://gleam.run/documentation/'>https://gleam.run/documentation/</a>
2. Fonctions de chaîne en Gleam: <a href='https://hexdocs.pm/gleam_stdlib/gleam/string.html'>https://hexdocs.pm/gleam_stdlib/gleam/string.html</a>
3. Introduction à la programmation Gleam: <a href='https://gleam.run/getting-started/'>https://gleam.run/getting-started/</a>