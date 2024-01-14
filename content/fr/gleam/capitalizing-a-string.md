---
title:                "Gleam: Capitalize une chaîne de caractères"
simple_title:         "Capitalize une chaîne de caractères"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Gleam, il y a de fortes chances que vous ayez déjà rencontré la nécessité de capitaliser une chaîne de caractères dans votre code. Cela peut sembler être une tâche simple, mais il y a quelques subtilités à prendre en compte pour capitaliser correctement une chaîne. Dans cet article, nous allons explorer pourquoi et comment capitaliser une chaîne en utilisant Gleam.

## Comment faire

Pour capitaliser une chaîne de caractères en Gleam, nous allons utiliser la fonction `String.capitalize`. Cette fonction prend une chaîne en entrée et renvoie cette même chaîne avec la première lettre en majuscule. Voyons un exemple de code pour mieux comprendre :

```Gleam
import gleam/string

let nom = "jean"
let nom_capitalisé = String.capitalize(nom)
```

Nous avons d'abord importé le module `gleam/string`, qui contient la fonction `capitalize` dont nous avons besoin. Ensuite, nous avons défini une chaîne de caractères `nom` avec la valeur "jean", puis nous avons appelé la fonction `capitalize` en utilisant cette chaîne et stocké le résultat dans la variable `nom_capitalisé`.

Lorsque nous exécutons ce code, nous obtenons la valeur `Jean` dans `nom_capitalisé`, ce qui montre que la fonction a bien capitalisé la chaîne `"jean"`. Facile, n'est-ce pas ?

## Plongée en profondeur

Mais ne vous arrêtez pas là ! La fonction `capitalize` de Gleam ne se contente pas de capitaliser la première lettre d'une chaîne. Elle est également capable de gérer les caractères spéciaux et les accents. Par exemple, si nous voulons capitaliser le prénom "élise" en utilisant notre fonction, le résultat sera bien `Élise` avec une majuscule accentuée.

En creusant un peu plus, nous découvrons également que la fonction `String.capitalize` prend en compte les caractères Unicode. Cela signifie que vous pouvez capitaliser des chaînes dans n'importe quelle langue, sans avoir à vous soucier des spécificités des différentes alphabets.

## Voir aussi

Maintenant que vous savez comment capitaliser une chaîne en Gleam, voici quelques liens utiles pour en apprendre davantage sur le langage :

- [Documentation officielle de Gleam](https://gleam.run/)
- [Guide de démarrage pour les débutants en Gleam](https://evuez.tech/blog/gleam-language-intro-beginner/)
- [Tutoriels vidéo pour apprendre Gleam](https://www.youtube.com/playlist?list=PLPtGE1CdUgIdQzo-dWff3JjNuXrWc9WbF)