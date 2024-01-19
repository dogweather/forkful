---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ? 

Convertir une date en chaîne de caractères consiste à transformer une structure de données de type date dans un langage de programmation en une chaîne de caractères. Les programmeurs le font pour faciliter l'affichage et le stockage qui sont souvent plus compatibles avec le type chaîne de caractères.

## Comment faire :

Dans Haskell, vous pouvez utiliser la bibliothèque `Data.Time` pour le faire. Voici un exemple de code :

```haskell
import Data.Time

dateToString :: IO String
dateToString = do
    current_time <- getCurrentTime
    let format = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S " current_time
    return format
```

Quand vous exécutez ce code, vous obtiendrez une sortie comme celle-ci : 

```haskell
"2022-03-07 14:30:55 "
```

## Plongée en profondeur :

Haskell n'a pas toujours eu une manipulation de date et d'heure intégrée, celle-ci a été introduite avec la bibliothèque `Data.Time` dans la version 6.6. Avant cela, les programmeurs devaient implémenter manuellement la conversion des dates en chaînes de caractères, souvent à travers des bibliothèques externes.

Comme alternative, vous pourriez opter pour la bibliothèque `time-format` qui vous permet d'utiliser des formats de style strftime pour convertir une date en chaîne de caractères. C'est une option si vous préférez un contrôle plus minutieux sur le formatage.

La fonction `formatTime` de la bibliothèque `Data.Time` prend deux arguments, une spécification de format qui détermine comment la date doit être convertie en chaîne de caractères, et le temps à formater. La fonction renvoie ensuite la chaîne de caractères formatée.

## Voir aussi :

1. [Data.Time - Haskell Documentation](http://hackage.haskell.org/package/time-1.11.1.1/docs/Data-Time.html)
2. [Time-format - Haskell Documentation](http://hackage.haskell.org/package/time-format)
3. [Introduction à Haskell - Conversion de données](https://www.lri.fr/~filliatr/ens/compil/s4.html)