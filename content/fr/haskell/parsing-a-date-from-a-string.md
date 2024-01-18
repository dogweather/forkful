---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
html_title:           "Haskell: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Le parsing d'une date à partir d'une chaîne de caractères est le processus de conversion d'une date sous forme de chaîne de caractères en une structure de données date qui peut être utilisée par un programme. Les programmeurs font cela pour pouvoir manipuler et utiliser des dates de manière plus efficace dans leurs programmes.

# Comment faire:

Voici un exemple de code en Haskell pour parser une date à partir d'une chaîne de caractères :

```Haskell
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar

parseDate :: String -> Maybe Day
parseDate input = parseTimeM True defaultTimeLocale "%m/%d/%Y" input

main :: IO ()
main = do
    putStrLn "Entrez une date au format m/j/a :"
    input <- getLine
    let maybeDate = parseDate input
    case maybeDate of
        Just date -> putStrLn $ "La date que vous avez entrée est : " ++ show date
        Nothing -> putStrLn "Le format de la date est incorrect. Veuillez réessayer."
```

Output (entrée : 06/10/2021):
La date que vous avez entrée est : 2021-06-10

Ce code utilise la fonction `parseTimeM` fournie par le module `Data.Time.Format` pour faire le parsing. On utilise le type `Maybe Day` pour gérer les cas où la date entrée n'est pas au bon format. Si la date est correcte, on l'affiche, sinon on affiche un message d'erreur.

# Un peu plus en détail:

En 1988, le format de date ISO 8601 a été publié et est rapidement devenu la norme pour représenter les dates dans les systèmes informatiques. Depuis lors, les programmeurs ont dû manipuler et convertir des dates dans des chaînes de caractères pour pouvoir les utiliser correctement.

En plus de la fonction `parseTimeM`, il existe d'autres méthodes pour parse des dates en Haskell, comme l'utilisation du module `Text.ParserCombinators.ReadP` ou en utilisant la bibliothèque `time` qui fournit également des fonctions pour manipuler des dates.

# Voir aussi:

- [Documentation du module Data.Time.Format](https://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Format.html)
- [Documentation du module Text.ParserCombinators.ReadP](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html)
- [Documentation de la bibliothèque time](https://hackage.haskell.org/package/time)