---
title:    "Elm: Afficher la sortie de débogage"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage est un excellent moyen de voir les données que votre code produit à chaque étape de son exécution. Cela peut vous aider à comprendre et à résoudre les erreurs et les bugs dans votre programme.

## Comment faire

Dans Elm, l'impression de sortie de débogage est facile à mettre en œuvre grâce à la fonction `Debug.log`. Cette fonction prend deux paramètres : une chaîne de caractères pour indiquer le nom de la sortie et une valeur à imprimer.

Voici un exemple simple d'utilisation de `Debug.log` pour imprimer un message de débogage et la valeur d'une variable :

```Elm
age : Int
age = 25

Debug.log "Âge actuel :" age
```

En exécutant ce code, vous verrez la sortie suivante dans la console :

```
Âge actuel : 25
```

Vous pouvez également utiliser `Debug.log` pour imprimer des valeurs plus complexes telles que des listes, des enregistrements ou des fonctions. Il suffit de les inclure en tant que deuxième paramètre de la fonction.

## Plongée profonde

L'impression de sortie de débogage peut être très utile pour comprendre le flux de données dans votre programme. Cependant, il est important de noter qu'il ne doit pas être utilisé en production, car cela pourrait révéler des informations sensibles et ralentir considérablement l'exécution de votre programme.

Il est également important de noter que les appels à `Debug.log` sont ignorés lorsque vous compilez en mode production. Cela signifie que vous n'aurez pas à vous soucier de supprimer ces appels avant de publier votre code.

## Voir aussi

- [La documentation officielle d'Elm pour la fonction Debug.log](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug#log)
- [Un tutoriel plus complet sur l'utilisation de l'impression de sortie de débogage en Elm](https://medium.com/@jgrenat/improve-your-elm-development-with-debug-print-statements-bf85e5389954)
- [Des conseils pour éviter la surutilisation de l'impression de sortie de débogage en Elm](https://medium.com/@zhengyi_goh/elm-debug-print-with-a-purpose-2f3282e098b5)