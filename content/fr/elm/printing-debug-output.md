---
title:                "Elm: Imprimer le débogage du code"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de données de débogage peut sembler fastidieuse et inutile au premier abord, mais c'est en réalité un outil précieux pour comprendre le fonctionnement de votre code. En imprimant des valeurs spécifiques à des moments précis de l'exécution, vous pouvez facilement repérer les erreurs et les problèmes potentiels dans votre programme.

## Comment faire

Pour imprimer des données de débogage en Elm, vous pouvez utiliser la fonction `Debug.log`. Cette fonction prend deux arguments : une chaîne de texte pour décrire la valeur que vous voulez imprimer, et la valeur elle-même. Voici un exemple :

```Elm
monChiffre = 42
Debug.log "Mon chiffre préféré :" monChiffre
-- Résultat dans la console : Mon chiffre préféré : 42
```

Vous pouvez également imprimer des valeurs plus complexes, comme des listes ou des enregistrements :

```Elm
maListe = [1, 2, 3]
Debug.log "Ma liste de nombres :" maListe
-- Résultat dans la console : Ma liste de nombres : [1, 2, 3]

monEnregistrement = { name = "Alice", age = 25 }
Debug.log "Mon enregistrement :" monEnregistrement
-- Résultat dans la console : Mon enregistrement : { name = "Alice", age = 25 }
```

## Plongée en profondeur

Pour des données de débogage plus avancées, vous pouvez utiliser la fonction `Debug.toString`. Cette fonction prend simplement une valeur en entrée et renvoie sa représentation sous forme de chaîne de texte.

Vous pouvez également utiliser la fonction `Debug.todo` pour créer un rappel pour vous-même de revenir et de traiter une partie de votre code plus tard. Par exemple :

```Elm
maListe = [1, 2, 3]
maListeInverse = List.reverse maListe |> Debug.todo "Gérer les cas où la liste est vide"
```

## Voir aussi

Pour en savoir plus sur l'impression de données de débogage en Elm, consultez les ressources suivantes :

- La documentation officielle d'Elm sur `Debug` : https://package.elm-lang.org/packages/elm/core/latest/Debug
- Un article de blog sur l'impression de données de débogage en Elm : https://dennisreimann.de/articles/elm-debugging.html