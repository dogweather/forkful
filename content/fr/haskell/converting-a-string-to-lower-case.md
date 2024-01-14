---
title:    "Haskell: Convertir une chaîne en minuscules"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
L'une des tâches courantes en programmation est de manipuler des chaînes de caractères. Une de ces manipulations peut être la conversion en minuscules, utile pour faciliter la comparaison de chaînes ou pour des raisons esthétiques. Dans cet article, nous allons explorer comment le faire en utilisant le langage Haskell.

## Comment Faire
Pour convertir une chaîne en minuscules en Haskell, nous pouvons utiliser la fonction `toLower` de la bibliothèque standard `Data.Char`. Voici un exemple de code :

```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = map toLower str

main = do
    let result = lowercase "BONJOUR"
    print result
```

La première ligne importe la fonction `toLower` de la bibliothèque standard. La fonction `lowercase` utilise la fonction `map` pour appliquer `toLower` à chaque caractère de la chaîne donnée en argument. Ensuite, nous appelons cette fonction avec la chaîne "BONJOUR" et imprimons le résultat. Le résultat attendu est "bonjour".

Nous pouvons également utiliser la même méthode en utilisant une liste de compréhension :

```Haskell
lowercase :: String -> String
lowercase str = [toLower c | c <- str]
```

## Plongée en Profondeur
Maintenant, nous allons plonger en profondeur et comprendre comment fonctionne la fonction `toLower`. En Haskell, les caractères sont représentés par des nombres, grâce à une extension du langage appelée Unicode. Ainsi, chaque caractère possède une valeur numérique qui lui correspond. Par exemple, la lettre "A" a la valeur 65 et la lettre "a" a la valeur 97. La fonction `toLower` utilise ces valeurs pour déterminer la version minuscule de chaque caractère.

Par exemple, voici comment nous pourrions implémenter la fonction `toLower` nous-mêmes :

```Haskell
toLowerChar :: Char -> Char
toLowerChar c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c
```

Cette fonction utilise la fonction `fromEnum` pour convertir un caractère en sa valeur numérique et la fonction `toEnum` pour convertir une valeur numérique en caractère. Nous ajoutons ensuite 32 à la valeur numérique si le caractère est une lettre majuscule pour obtenir sa version minuscule (puisque les lettres majuscules et minuscules ont une différence de 32 en valeur numérique).

## Voir Aussi
- [Documentation de la fonction `toLower`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#v:toLower)
- [Documentation de la fonction `map`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
- [Documentation de la liste de compréhension](https://wiki.haskell.org/List_comprehension)
- [Documentation sur l'Unicode en Haskell](https://wiki.haskell.org/Roadmap/Compatibility/Unicode)