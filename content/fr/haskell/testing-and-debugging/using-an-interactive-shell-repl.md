---
title:                "Utilisation d'une console interactive (REPL)"
aliases:
- /fr/haskell/using-an-interactive-shell-repl/
date:                  2024-01-26T04:14:45.200745-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un shell interactif, ou REPL (Read-Eval-Print Loop), en Haskell vous permet d'exécuter des extraits de code en direct. C'est un terrain de jeu pour obtenir des retours rapides, tester des fonctions et apprendre le langage.

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
