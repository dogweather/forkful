---
date: 2024-01-26 01:06:55.335383-07:00
description: "La journalisation en programmation consiste essentiellement \xE0 laisser\
  \ une trace de miettes de pain sous forme d'\xE9v\xE9nements ou de messages enregistr\xE9\
  s, qui\u2026"
lastmod: '2024-03-13T22:44:57.840616-06:00'
model: gpt-4-1106-preview
summary: "La journalisation en programmation consiste essentiellement \xE0 laisser\
  \ une trace de miettes de pain sous forme d'\xE9v\xE9nements ou de messages enregistr\xE9\
  s, qui peuvent \xEAtre utilis\xE9s pour suivre ce que fait votre application \xE0\
  \ tout moment."
title: Journalisation
weight: 17
---

## Comment faire :
En Haskell, la journalisation peut être implémentée en utilisant des bibliothèques telles que `monad-logger` ou `hslogger`. Voici un exemple rapide en utilisant `monad-logger` :

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "Démarrage de l'application..."
    liftIO $ putStrLn "Exécution de certains travaux critiques..."
    logErrorN "Oups ! Quelque chose s'est mal passé."

main :: IO ()
main = runStdoutLoggingT logExample

{- Sortie d'exemple
[Info] Démarrage de l'application...
Exécution de certains travaux critiques...
[Error] Oups ! Quelque chose s'est mal passé.
-}
```

Cet exemple simple illustre comment vous pouvez parsemer des instructions de journalisation dans votre code pour obtenir des aperçus de ce qui se passe lors de l’exécution. `logInfoN` et `logErrorN` sont utilisés pour consigner respectivement des messages d'information et d'erreur.

## Plongée en profondeur :
La journalisation a parcouru un long chemin depuis de simples instructions d’impression jusqu’à des cadres de journalisation sophistiqués. Historiquement, les journaux n'étaient que des sorties de texte vers une console ou un fichier, mais maintenant ils incluent des données structurées qui peuvent être analysées et interprétées par divers outils.

En Haskell, la journalisation peut être réalisée dans un style purement fonctionnel qui implique de passer explicitement des actions de journalisation ou d'utiliser des contextes monadiques pour l'impureté, où les loggers sont implicitement intégrés dans le calcul.

La bibliothèque `hslogger`, par exemple, est plus traditionnelle et mutable comparée à `monad-logger`. `monad-logger` offre une intégration avec la pile de monades et fournit plus de flexibilité en termes de formatage de la sortie et de contrôle. Les deux bibliothèques vous permettent de définir des niveaux de journalisation, qui aident à filtrer les messages de journalisation basés sur leur importance. Les niveaux de journalisation incluent debug, info, notice, warning, error, critical, alert, et emergency.

L'approche de Haskell à la journalisation s'aligne souvent sur son accent sur la sécurité des types et la pureté. Les logs peuvent être gérés de telle manière que même si la journalisation échoue, cela ne provoquera pas l'arrêt de l'application principale grâce aux capacités robustes de gestion des erreurs de Haskell.

## Voir aussi :
- [Documentation de `monad-logger` sur Hackage](https://hackage.haskell.org/package/monad-logger)
- [Package `hslogger` sur Hackage](https://hackage.haskell.org/package/hslogger)
- [Real World Haskell, Chapitre 19, sur la gestion des erreurs](http://book.realworldhaskell.org/read/error-handling.html)
- [La façade de journalisation pour Haskell (log-base)](https://hackage.haskell.org/package/log-base)
