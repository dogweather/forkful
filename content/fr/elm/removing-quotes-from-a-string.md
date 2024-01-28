---
title:                "Retirer les guillemets d'une chaîne"
date:                  2024-01-26T03:38:45.356800-07:00
model:                 gpt-4-0125-preview
simple_title:         "Retirer les guillemets d'une chaîne"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Enlever les guillemets d'une chaîne signifie supprimer ces marques de citations doubles ou simples supplémentaires dont vous n'avez pas réellement besoin dans le texte traité. Les programmeurs font cela pour assainir l'entrée, préparer les données pour le stockage ou rendre la sortie plus lisible pour l'humain lorsque les guillemets ne sont pas nécessaires dans le contexte donné.

## Comment faire :
En Elm, vous pouvez utiliser les fonctions `String` pour manipuler des chaînes de caractères, telles que l'enlèvement des guillemets. Voici une manière simple de le faire :

```Elm
removeQuotes : String -> String
removeQuotes str =
    String.trim (String.filter (\char -> char /= '\"' && char /= '\'') str)

main =
    String.removeQuotes "\"Ceci est une chaîne 'citée' !\""
    -- Sortie: Ceci est une chaîne citée !
```

Rappelez-vous juste : ce petit extrait va enlever tous les guillemets de votre chaîne, donc utilisez-le judicieusement !

## Plongée en Profondeur
Autrefois, traiter avec des chaînes était un peu plus manuel, impliquant beaucoup d'analyse manuelle. De nos jours, des langues comme Elm le rendent plus simple avec des fonctions intégrées. La fonction `String.filter` est un outil polyvalent dans votre arsenal pour lorsque vous avez besoin de vous soucier de chaque caractère, ce qui inclut mais ne se limite pas à arracher des guillemets.

Comme alternative, vous pourriez vous tourner vers les expressions régulières si Elm les supportait de manière portable, ce qu'il ne fait pas par défaut. Mais hé, l'accent mis par Elm sur la simplicité et la sécurité signifie que notre approche `String.filter` est claire, sûre et facile à maintenir.

L'approche fonctionnelle d'Elm encourage des fonctions pures sans effets secondaires, et `removeQuotes` en est un excellent exemple. Elle prend une chaîne et en retourne une nouvelle, laissant l'originale intacte. C'est le jeu des structures de données immuables d'Elm, favorisant la prévisibilité et soulageant vos maux de tête de débogage.

## Voir Aussi
Pour des lectures complémentaires et des aventures liées à la manipulation de chaînes, consultez la documentation du module `String` d'Elm à :

- [Docs des Chaînes Elm](https://package.elm-lang.org/packages/elm/core/latest/String)

Et si vous vous trouvez dans une impasse concernant ce qu'Elm supporte en termes de manipulation de chaînes ou toute autre fonctionnalité du langage :

- [Guide du langage Elm](https://guide.elm-lang.org/)
