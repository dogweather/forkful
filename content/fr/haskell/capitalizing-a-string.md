---
title:    "Haskell: Capitaliser une chaîne de caractères"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation Haskell, vous vous demandez peut-être pourquoi nous voudrions capitaliser une chaîne de caractères. La réponse est simple : capitaliser une chaîne de caractères est souvent nécessaire pour des raisons esthétiques ou fonctionnelles dans de nombreux programmes.

## Comment faire

La fonction "toUpper" est utilisée pour capitaliser une lettre individuelle dans une chaîne de caractères. En utilisant cette fonction avec la fonction "map", nous pouvons appliquer la mise en majuscule à chaque lettre d'une chaîne de caractères. Voici un exemple de code :

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str

main = do
  putStrLn $ capitalize "bonjour le monde" 
```

Lorsque vous exécutez ce code, vous devriez obtenir l'output suivant : "BONJOUR LE MONDE". Vous pouvez également utiliser cette fonction pour capitaliser seulement la première lettre d'une chaîne de caractères en utilisant la fonction "head" pour sélectionner le premier élément et "tail" pour les éléments restants. Voici un autre exemple de code :

```Haskell
firstCapital :: String -> String
firstCapital str = toUpper (head str) : tail str

main = do
  putStrLn $ firstCapital "bonjour le monde" 
```

L'output de ce code sera "Bonjour le monde".

## Plongeon profond

Maintenant que vous savez comment capitaliser une chaîne de caractères en Haskell, il est important de comprendre comment la fonction "map" fonctionne réellement. En Haskell, les fonctions sont des valeurs de première classe, ce qui signifie qu'elles peuvent être traitées comme n'importe quelle autre valeur. La fonction "map" prend en paramètre une autre fonction (dans ce cas, "toUpper") et l'applique à chaque élément d'une liste. Cela signifie que la fonction "map toUpper" peut être utilisée pour plus que simplement capitaliser une chaîne de caractères, elle peut également être utilisée pour transformer toutes sortes de données comme des listes de nombres ou des listes de booléens.

## Voir aussi

Pour plus d'informations sur la fonction "map" et d'autres fonctions utiles en Haskell, vous pouvez consulter les ressources suivantes :

- [Introduction à Haskell](https://www.haskell.org/)
- [Documentation officielle de Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
- [Tutoriel vidéo sur Haskell](https://www.youtube.com/watch?v=a1EU3exFS2s) (en français)