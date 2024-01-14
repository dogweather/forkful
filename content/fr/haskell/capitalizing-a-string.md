---
title:    "Haskell: Capitalize une chaîne de caractères"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Lorsque l'on travaille avec des chaînes de caractères en Haskell, il peut être utile d'avoir la fonction de convertir une chaîne en majuscules pour des raisons d'affichage ou de traitement de données. Dans cet article, nous allons expliquer comment capitaliser une chaîne de caractères en Haskell.

## Comment faire

En Haskell, la fonction `toUpper` du module `Data.Char` permet de convertir un caractère en majuscule. Pour capitaliser une chaîne de caractères entière, nous allons utiliser la fonction `map` qui applique une opération sur chaque élément d'une liste. Dans notre cas, nous allons appliquer `toUpper` à chaque caractère de la chaîne. Voici un exemple concret :

```Haskell
import Data.Char (toUpper)

-- Définition de la fonction capitalize
capitalize :: String -> String
capitalize string = map toUpper string

main = do
    let myString = "salut le monde"
    putStrLn (capitalize myString)
```

La sortie de ce code sera :

```
SALUT LE MONDE
```

## Plongée en profondeur

Il est important de noter que la fonction `capitalize` que nous avons définie est sensiblement différente de la fonction `toUpper` de la bibliothèque standard de Haskell. La fonction `toUpper` s'attend à un seul caractère en entrée et renvoie un seul caractère en sortie, tandis que notre fonction `capitalize` travaille sur une chaîne de caractères complète.

De plus, notre fonction `capitalize` est limitée aux caractères ASCII et ne fonctionnera pas avec des caractères Unicode. Pour une solution plus complète, il est recommandé d'utiliser le module `Data.Text` qui offre des fonctions de manipulation de texte plus avancées et prenant en charge les caractères Unicode.

## Voir aussi

- Documentation officielle de la fonction `toUpper` : https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toUpper
- Documentation officielle du module `Data.Text` : https://hackage.haskell.org/package/text
- Guide d'apprentissage Haskell : https://learnxinyminutes.com/docs/fr-fr/haskell-fr/