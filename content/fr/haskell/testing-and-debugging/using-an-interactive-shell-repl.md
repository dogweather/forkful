---
date: 2024-01-26 04:14:45.200745-07:00
description: "Comment faire : Pour d\xE9marrer le GHCi (l'environnement interactif\
  \ du compilateur Haskell de Glasgow), tapez simplement `ghci` dans votre terminal.\
  \ Voici\u2026"
lastmod: '2024-03-13T22:44:57.835668-06:00'
model: gpt-4-0125-preview
summary: "Pour d\xE9marrer le GHCi (l'environnement interactif du compilateur Haskell\
  \ de Glasgow), tapez simplement `ghci` dans votre terminal."
title: Utilisation d'une console interactive (REPL)
weight: 34
---

## Comment faire :
Pour démarrer le GHCi (l'environnement interactif du compilateur Haskell de Glasgow), tapez simplement `ghci` dans votre terminal. Voici comment l'utiliser :

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

La sortie d'exemple explique que `x` est une variable numérique et montre que son double résulte en 10.

## Exploration approfondie :
Le GHCi de Haskell a beaucoup évolué depuis sa création. Il offre un riche ensemble de fonctionnalités telles que l'achèvement automatique, l'entrée sur plusieurs lignes et le chargement de paquets. Des alternatives comme Hugs sont aujourd'hui principalement historiques, avec GHCi étant la norme. GHCi compile le code juste-à-temps chaque fois que vous entrez une expression, vous offrant ainsi une manière efficace de tester votre code Haskell.

## Voir aussi :
- [Le guide de l'utilisateur de GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Apprenez-vous un Haskell pour un grand bien ! – Commencer](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
