---
title:                "Écrire vers l'erreur standard"
html_title:           "Elm: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Ecrire vers le standard error (erreur standard) en programmation Elm signifie écrire des messages d'erreur qui peuvent être affichés lors de l'exécution de votre code. Les programmeurs le font pour débugger leurs applications et pour obtenir des informations précises sur les erreurs qui se produisent. 

## Comment faire:

Il existe une manière simple de rédiger vers le standard error en Elm en utilisant la fonction `Debug.log`. Voici un exemple de code:

```Elm
main = Debug.log "Message d'erreur" (toString 10)
```

Ce code va écrire "Message d'erreur : 10" dans le standard error lorsque le programme s'exécute. Notez que la fonction `toString` sert à convertir le nombre 10 en une chaîne de caractères pour pouvoir l'afficher dans le message. Vous pouvez aussi utiliser `String.fromInt` pour convertir un entier en chaîne de caractères.

## Plongée plus profonde:

L'écriture vers le standard error a été introduite pour la première fois dans le langage Elm dans sa version 0.18. Avant cela, les programmeurs utilisaient `Debug.log` pour écrire vers la console, ce qui pouvait être confus car les messages étaient affichés dans le navigateur plutôt que dans l'éditeur de code.

Une alternative à l'utilisation de `Debug.log` est l'utilisation du débogage à l'aide d'un débogueur. Cela peut être utile pour les applications plus complexes où l'utilisation de `Debug.log` peut rapidement devenir fastidieuse. Cependant, cela nécessite des connaissances supplémentaires et peut être plus complexe à mettre en place.

## A voir aussi:

Pour en savoir plus sur l'écriture vers le standard error en Elm, vous pouvez consulter la documentation officielle sur `Debug.log` ainsi que sur le débogage en général. Vous pouvez également explorer les différentes méthodes de débogage en Elm et décider quelle méthode convient le mieux à vos besoins.