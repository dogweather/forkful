---
date: 2024-01-26 03:40:36.766010-07:00
description: "Comment faire : En Haskell, nous pouvons concocter une fonction qui\
  \ supprime tous les guillemets d'une cha\xEEne donn\xE9e. C'est comme dire aux guillemets\
  \ de\u2026"
lastmod: '2024-03-13T22:44:57.821226-06:00'
model: gpt-4-0125-preview
summary: "En Haskell, nous pouvons concocter une fonction qui supprime tous les guillemets\
  \ d'une cha\xEEne donn\xE9e."
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Comment faire :
En Haskell, nous pouvons concocter une fonction qui supprime tous les guillemets d'une chaîne donnée. C'est comme dire aux guillemets de déguerpir, et de s'assurer qu'ils saisissent l'allusion.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell a dit, \"Apprenons quelques fonctions !\""
    putStrLn $ removeQuotes stringWithQuotes
```

Exemple de sortie :

```
Haskell a dit, Apprenons quelques fonctions !
```

## Exploration Approfondie
Il fut un temps, avant que les chaînes de programmation ne soient aussi communes que les vidéos de chats sur internet, manipuler du texte était une affaire délicate. Mais à mesure que les langages de programmation évoluaient, les chaînes devenaient une partie cruciale du codage. Pourtant, les guillemets sont restés une épée à double tranchant—essentiels pour définir les chaînes, mais une nuisance lorsqu'inclus comme données réelles.

Des alternatives ? Au lieu de repousser tous les guillemets comme des mouches, vous pouvez être sélectif. Vous voudrez peut-être supprimer uniquement les guillemets les plus externes (une coupe classique) ou gérer les guillemets échappés à l'intérieur d'une chaîne.

En termes d'implémentation, la fonction `removeQuotes` ci-dessus utilise une lambda pour vérifier chaque caractère (`c`) pour voir si c'est un guillemet importun et les filtre en conséquence. C'est une approche directe, mais pour des textes plus volumineux ou des règles plus complexes, vous voudrez peut-être examiner les bibliothèques d'analyse syntaxique comme `Parsec` qui peuvent vous offrir plus de finesse et de puissance dans le traitement du texte.

## Voir Aussi :
- Pour les amateurs de regex : [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Une introduction douce aux chaînes de caractères en Haskell : [Apprenez-vous un Haskell pour un grand bien ! - Commencer](http://learnyouahaskell.com/starting-out#strings)
