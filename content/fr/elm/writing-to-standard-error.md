---
title:                "L'écriture dans la sortie d'erreur standard"
html_title:           "Elm: L'écriture dans la sortie d'erreur standard"
simple_title:         "L'écriture dans la sortie d'erreur standard"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm passionné, vous savez déjà que l'une de ses forces est la gestion des erreurs. Cela signifie que votre application Elm sera moins susceptible de planter ou de générer des erreurs imprévues. Cependant, il peut être utile de comprendre comment écrire dans la sortie d'erreur standard pour déboguer votre code et détecter les erreurs plus rapidement.

## Comment procéder

Pour écrire dans la sortie d'erreur standard, tout ce que vous avez à faire est d'utiliser la fonction `Debug.crash` avec une chaîne de caractères en tant que paramètre. Cette fonction provoque une erreur et affiche la chaîne de caractères dans la sortie d'erreur standard. Voici un exemple de code pour illustrer cela :

```elm
import Debug exposing (crash)

main =
  crash "Une erreur est survenue"
```

Lorsque vous exécutez ce code, vous devriez voir "Une erreur est survenue" s'afficher dans la sortie d'erreur standard. Cela peut être utile pour déterminer où se situe une erreur dans votre code et pour vous aider à la corriger.

## Explorer en profondeur

En plus de la fonction `Debug.crash`, nous pouvons également utiliser la fonction `Debug.log` pour écrire dans la sortie d'erreur standard. Cette fonction prend deux paramètres : une chaîne de caractères et une valeur. Elle affichera la chaîne de caractères suivie de la valeur dans la sortie d'erreur standard. Voici un exemple :

```elm
import Debug exposing (log)

main =
  log "La longueur de la chaîne est : " (String.length "Elm est génial !")
```

Cela affichera "La longueur de la chaîne est : 16" dans la sortie d'erreur standard. Vous pouvez également utiliser la fonction `toString` pour afficher des valeurs de types différents dans la sortie d'erreur standard. Par exemple :

```elm
import Debug exposing (log)

main =
  log "La valeur de mon entier est : " (toString 10)
```

Cela affichera "La valeur de mon entier est : 10" dans la sortie d'erreur standard.

## Voir aussi 

- [`Debug` module in the Elm documentation](https://package.elm-lang.org/packages/elm/core/latest/)
- [Article sur la gestion des erreurs en Elm](https://www.parlez-vous.org/2017/05/29/error-handling-in-elm/)