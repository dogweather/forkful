---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'affichage des sorties de débogage, c'est écrire des informations de contrôle qui sont affichées pendant ou après l'exécution du programme. Les programmeurs le font pour comprendre comment leur programme fonctionne réellement, inspecter les bugs et vérifier la logique.

## Comment faire :
Voici comment vous pouvez utilisez `Debug.log` en Elm pour afficher des sorties de débogage.

```Elm
import Html exposing (text)
import Debug

main =
    text (Debug.log "This is logged" "that the function will display")
```
La fonction `Debug.log` prend deux arguments : le message à enregistrer et la valeur à afficher. Le programme ci-dessus générera la sortie de débogage suivante dans la console de votre navigateur :

```Elm
This is logged : that the function will display
```

## Deep Dive :
Historiquement parlant, l'affichage de la sortie de débogage est une pratique qui remonte aux premiers jours de la programmation. C'est une technique de débogage simple mais puissante.

En Elm, l'alternatives à `Debug.log` peut être `Debug.todo` qui arrête l'exécution du programme avec une erreur spécifique. Autre différente technique inclue l'usage de `Debug.toString` qui convertit une expression complexe en une chaîne de caractères.

Côté implementation, `Debug.log` utilise le système de console de JavaScript sous-jacent pour afficher les informations de débogage.

## Voir aussi : 
* Documentation de Elm sur la débogage : https://guide.elm-lang.org/effects/log.html
* Discussion sur les différentes techniques de débogage en Elm : https://stackoverflow.com/questions/35525514/how-to-print-to-the-console-in-elm