---
title:    "Haskell: Calculer une date dans le futur ou le passé"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi

En programmation, il peut être utile de savoir comment calculer une date dans le futur ou dans le passé, par exemple pour planifier des tâches ou des événements. Dans cet article, nous allons explorer comment le faire en Haskell.

# Comment faire

Premièrement, nous avons besoin d'importer le module `Data.Time` qui contient des fonctions pour manipuler les dates et les heures.

```
```Haskell
import Data.Time
```

Pour calculer une date dans le futur, nous pouvons utiliser la fonction `addDays` et lui donner un nombre de jours à ajouter. Voici un exemple calculant la date dans 10 jours:

```
```Haskell
let futur = addDays 10 (fromGregorian 2021 08 31)
```

Nous pouvons également calculer une date dans le passé en utilisant la fonction `addDays` avec un nombre de jours négatif. Par exemple, pour calculer la date 5 jours avant le 1er janvier 2022:

```
```Haskell
let passe = addDays (-5) (fromGregorian 2022 01 01)
```

Ensuite, nous pouvons utiliser la fonction `show` pour afficher la date dans un format lisible par l'humain:

```
```Haskell
putStrLn $ "La date dans 10 jours sera : " ++ show futur
```

Le code complet serait le suivant:

```
```Haskell
import Data.Time

main = do
    let futur = addDays 10 (fromGregorian 2021 08 31)
        passe = addDays (-5) (fromGregorian 2022 01 01)
    putStrLn $ "La date dans 10 jours sera : " ++ show futur
    putStrLn $ "La date 5 jours avant le 1er janvier 2022 sera : " ++ show passe
```

Et voici le résultat:

```
La date dans 10 jours sera : 2021-09-10
La date 5 jours avant le 1er janvier 2022 sera : 2021-12-27
```

# Plongée en profondeur

En plus de `addDays`, le module `Data.Time` offre également d'autres fonctions utiles pour manipuler les dates, telles que `addMonths` pour ajouter des mois, `addGregorianMonthsClip` pour ajouter des mois en limitant au dernier jour du mois, et `addGregorianYearsClip` pour ajouter des années en limitant au dernier jour du mois de février.

De plus, nous pouvons également utiliser la fonction `diffDays` pour calculer le nombre de jours entre deux dates, ou encore `diffUTCTime` pour calculer la différence entre deux temps universels coordonnés.

Explorez la documentation pour découvrir toutes les autres fonctions disponibles dans le module `Data.Time`.

# Voir aussi

- [Documentation sur le module Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html) 
- [Tutoriel sur la manipulation des dates en Haskell](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)