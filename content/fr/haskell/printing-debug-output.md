---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'impression de débogage est une méthode utilisée pour tracer les valeurs des variables en cours d'exécution. C'est essentiel pour comprendre et résoudre les problèmes de logique dans votre code.

## Comment:

On peut utiliser la fonction `print` de Haskell pour imprimer la sortie de débogage. Par exemple :

```Haskell
main = do  
    print "Quelle est ton année de naissance?"
    année <- getLine
    print ("Tu es né en " ++ année)
```

Voici ce que donnera la sortie :

```Haskell
"Quelle est ton année de naissance?"
"Tu es né en 1990"
```

## Plongée Profonde

Historiquement, imprimer la sortie de débogage est l'une des premières méthodes utilisées par les programmeurs pour déboguer le code. Bien que simple, elle est toujours utilisée de nos jours surtout pour les programmes simples ou pour une détection rapide des erreurs.

Les alternatives à l'impression de la sortie de débogage incluent l'utilisation d'un débogueur intégré, comme GHCi pour Haskell, qui permet à un développeur de pause et de reprendre l'exécution du code, d'examiner les valeurs et les types de variables et bien plus encore.

Côté mise en œuvre, alors que `print` imprime une valeur à l'écran, Haskell offre plusieurs autres fonctions pour personnaliser l'impression de débogage. Par exemple, `trace` et `traceShow` du module Debug.Trace qui permettent d'insérer des messages de débogage dans vos fonctions sans pour autant interrompre leur exécution.

## Voir Aussi

Pour en savoir plus sur le débogage en Haskell, jetez un coup d'œil à ces ressources :

- Le guide du débogage de GHC: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/debug-info.html
- Un excellent tutoriel sur le débogage de Haskell: https://wiki.haskell.org/Debugging
- Une discussion sur la meilleure façon de déboguer en Haskell: https://stackoverflow.com/questions/10199161/debugging-in-haskell