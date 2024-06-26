---
date: 2024-01-26 03:49:16.548208-07:00
description: "Comment faire : Prenons une promenade avec GHCi, l'environnement interactif\
  \ de Haskell qui peut agir comme un d\xE9bogueur de base. Vous le lancez avec votre\u2026"
lastmod: '2024-03-13T22:44:57.838664-06:00'
model: gpt-4-0125-preview
summary: "Prenons une promenade avec GHCi, l'environnement interactif de Haskell qui\
  \ peut agir comme un d\xE9bogueur de base."
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Prenons une promenade avec GHCi, l'environnement interactif de Haskell qui peut agir comme un débogueur de base. Vous le lancez avec votre code Haskell et commencez à explorer. Voici un exemple :

```Haskell
main :: IO ()
main = do
    putStrLn "Hey, c'est quoi ton nom ?"
    name <- getLine
    putStrLn $ "Bonjour, " ++ name ++ "! Allons déboguer."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Faites semblant qu'il y a un bug ici
```

Pour commencer à déboguer avec GHCi :

```bash
$ ghci VotreFichierHaskell.hs
```

Mettez un point d'arrêt sur `buggyFunction` :

```Haskell
Prelude> :break buggyFunction
```

Exécutez votre programme :

```Haskell
Prelude> :main
Hey, c'est quoi ton nom ?
```

Votre programme se met en pause sur `buggyFunction`. Vous pouvez maintenant inspecter les variables, parcourir le code pas à pas, et évaluer des expressions.

## Plongée profonde :
Historiquement, la réputation de Haskell pour ses fonctions pures et son typage fort a conduit à croire que les outils de débogage étaient moins critiques. La réalité est différente - les programmes complexes bénéficient toujours de bons outils de débogage. GHCi fournit des commandes de débogage de base. Cependant, pour une expérience plus visuelle ou pour des applications à plus grande échelle, vous pourriez explorer des IDE avec des débogueurs intégrés, comme Visual Studio Code avec des extensions Haskell ou le plugin Haskell d'IntelliJ.

Les alternatives au débogueur incluent l'utilisation d'instructions print, connues sous le nom de "débogage printf", ou l'exploitation du système de typage fort de Haskell pour rendre les états incorrects non représentables. Pourtant, parfois, rien ne remplace le fait de passer à travers le code pas à pas.

En ce qui concerne les détails d'implémentation, le débogueur de Haskell fonctionne avec le système d'exécution. Il peut gérer les points d'arrêt, l'exécution pas à pas et permettre l'inspection des variables. Cependant, puisque Haskell est évalué paresseusement, les choses peuvent devenir un peu contre-intuitives. Déboguer un programme Haskell signifie souvent garder un œil sur quand et comment les expressions sont évaluées.

## Voir aussi :
- [Guide de l'utilisateur GHC - Débogueur](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Plugin Haskell IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
