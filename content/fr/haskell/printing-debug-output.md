---
title:    "Haskell: Impression de messages de débogage"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous été confrontés à des problèmes lors de l'écriture de programmes Haskell. Peut-être qu'une fonction ne renvoie pas la valeur attendue ou qu'une boucle ne s'exécute pas comme prévu. Dans ces cas-là, il peut être très utile d'utiliser l'impression de messages de debug dans notre code. Cela nous permet de voir les valeurs de nos variables à des points spécifiques de l'exécution du programme, nous aidant ainsi à identifier et à résoudre les problèmes plus rapidement.

## Comment faire

L'impression de messages de debug en Haskell est très simple. Tout ce que nous avons à faire est d'utiliser la fonction `print` pour afficher les valeurs de nos variables. Regardons un exemple concret :

```Haskell
-- Définir une fonction qui multiplie chaque élément d'une liste par 2
exemple :: [Int] -> [Int]
exemple list = go list []
    where go [] acc = acc
          go (x:xs) acc = go xs (acc ++ [x * 2])

-- Appeler la fonction avec une liste de nombres
main :: IO ()
main = do
    let list = [1, 2, 3, 4, 5]
    print (exemple list)
```

Dans cet exemple, nous créons une fonction appelée `exemple` qui prend en paramètre une liste d'entiers et multiplie chaque élément de cette liste par 2. Ensuite, dans la fonction `main`, nous appelons notre fonction et nous imprimons le résultat avec la fonction `print`.

Si nous exécutons ce code, nous obtiendrons la sortie suivante :

```
[2,4,6,8,10]
```

Nous pouvons voir que notre fonction a bien multiplié chaque élément de la liste par 2. Cela pourrait être utile si nous voulons vérifier si notre fonction fonctionne correctement sur des valeurs plus complexes ou si nous avons besoin de comprendre pourquoi elle ne renvoie pas la valeur attendue.

## Deep Dive

L'impression de messages de debug peut également être utile pour comprendre le fonctionnement de notre code à un niveau plus profond. Par exemple, si nous avons une fonction récursive, il peut être difficile de comprendre comment elle s'exécute à chaque étape. En imprimant des messages de debug à chaque appel de la fonction, nous pouvons suivre son exécution et mieux comprendre son fonctionnement.

Il est également important de se rappeler de supprimer les messages de debug une fois que nous avons résolu le problème. Le code avec de nombreux messages de debug peut être difficile à lire et à maintenir à long terme.

## À lire également

- [2 Simple and Useful Debugging Techniques in Haskell](https://hackhands.com/haskell-debugging-techniques/)
- [Debugging in Haskell](http://dev.stephendiehl.com/hask/#profiling)
- [How to Debug Haskell Like a Pro](https://medium.com/avi-on-code/haskell-debugging-like-a-pro-7ac2d5c27483)