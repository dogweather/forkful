---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Lancement d'un nouveau projet en Haskell 

## Quoi & Pourquoi?
Démarrer un nouveau projet est l'acte de mettre en place une base solide pour votre programme. Les programmeurs font cela pour organiser leurs pensées, définir le cadre du travail à faire, et commencer le développement avec une vision claire.

## Comment faire:
Pour commencer un nouveau projet Haskell, vous pouvez utiliser `stack` ou `cabal`. Voici un exemple d'utilisation de `stack`:

```Haskell
stack new my-project simple
cd my-project
stack setup
stack build
stack exec my-project-exe
```

Vous devriez voir `hello world` comme sortie. Cela signifie que vous avez réussi à démarrer votre nouveau projet !

## Exploration en profondeur
Historiquement, `stack` et `cabal` sont les deux principaux outils pour gérer les projets Haskell, chacun ayant ses propres avantages et inconvénients. `stack` est plus récent et est conçu pour être plus facile à utiliser, tandis que `cabal` est plus ancien et a une plus grande flexibilité. En fonction de vos besoins, vous pouvez choisir l'un ou l'autre pour gérer votre projet.

En ce qui concerne l'implémentation, quand vous lancez `stack new`, `stack` crée une série de fichiers et de dossiers pour votre projet, dont le plus important est le fichier `package.yaml`. Ce fichier définit tous les détails spécifiques du projet, y compris les dépendances.

## Voir Aussi
Pour plus d'informations, consultez les pages de documentation officielles pour [`stack`](https://docs.haskellstack.org/en/stable/README/) et [`cabal`](https://www.haskell.org/cabal/users-guide/). Vous pouvez également trouver utile le livre ["Haskell Programming from First Principles"](http://haskellbook.com/) qui a un excellent chapitre sur la configuration et la gestion des projets.