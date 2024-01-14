---
title:    "Elm: Affichage des données de débogage"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage peut sembler être une tâche ennuyeuse et fastidieuse, mais elle peut en fait être très utile dans le processus de développement de votre application Elm. Elle peut vous aider à comprendre le fonctionnement de votre code et à résoudre des problèmes plus rapidement.

## Comment faire

Pour imprimer la sortie de débogage dans votre code Elm, vous pouvez utiliser la fonction `Debug.log` en lui passant deux arguments : une chaîne de caractères pour décrire l'information que vous voulez imprimer et la valeur de l'expression à imprimer. Par exemple :

```
Elm.debug
    "Le nombre d'utilisateurs est : "
    (List.length utilisateurs)
```

Cela imprimera la phrase "Le nombre d'utilisateurs est : 10" si votre liste `utilisateurs` comprend 10 éléments.

## Plongée en profondeur

Il existe différents niveaux de débogage que vous pouvez utiliser : `Debug.log`, `Debug.log2`, `Debug.log3`, etc. Chaque niveau correspond à un nombre spécifique d'arguments que vous pouvez lui passer. Par exemple, `Debug.log2` accepte deux arguments, `Debug.log3` en accepte trois, et ainsi de suite.

De plus, vous pouvez également utiliser la fonction `Debug.todo` pour imprimer un message dans la console de débogage sans quitter votre application. Cela peut être utile lorsque vous voulez laisser un rappel à vous-même ou à vos coéquipiers pour implémenter une fonctionnalité manquante plus tard.

## Voir aussi

- Documentation officielle d'Elm sur le débogage : https://guide.elm-lang.org/debugging/
- Tutoriel vidéo sur l'utilisation du débogage dans Elm : https://www.youtube.com/watch?v=0DE5WaQOWtg