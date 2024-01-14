---
title:                "Haskell: Afficher la sortie de débogage"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi
Les messages de débogage sont un outil précieux pour tout programmeur, surtout lorsqu'il s'agit de langages fonctionnels comme Haskell. Ils nous permettent de comprendre l'exécution de notre code et de détecter les erreurs plus rapidement. Dans cet article, nous allons explorer comment imprimer des messages de débogage en Haskell pour améliorer notre processus de développement.

## Comment
Nous pouvons utiliser la fonction `Debug.Trace.trace` pour imprimer des messages de débogage dans notre code en Haskell. Voici un exemple de code qui imprime un message de débogage et retourne le résultat d'une opération :

```Haskell
-- importons le module Debug.Trace
import Debug.Trace

-- fonction pour additionner deux nombres et imprimer un message de débogage
addition :: Int -> Int -> Int
addition x y =
  let
    -- définissons notre message de débogage
    message = "On ajoute " ++ show x ++ " et " ++ show y
  in
    -- imprimons le message de débogage avec trace et retournons le résultat de l'addition
    trace message (x + y)
```

Si nous utilisons cette fonction dans notre code comme ceci :

```Haskell
resultat = addition 2 3
```

Nous obtiendrons le résultat suivant :

```
On ajoute 2 et 3
5
```

Nous pouvons voir que notre message de débogage a été imprimé avant le résultat de l'addition. Cela nous permet de suivre l'exécution de notre code et de comprendre ce qui se passe à chaque étape.

## Plongée en profondeur
La fonction `trace` prend deux arguments : le message de débogage et la valeur à retourner. Elle renvoie la valeur passée en argument en deuxième position, ce qui nous permet d'utiliser cette fonction dans n'importe quel contexte. Cela signifie que nous pouvons l'utiliser dans des fonctions récursives, des fonctions à haute ordre ou même dans des expressions lambda.

Il est également important de noter que la fonction `trace` n'imprimera pas le message de débogage si notre programme est compilé avec l'option `-O` pour optimiser le code. Cela peut être utile si nous voulons éviter d'imprimer des messages de débogage dans notre code en production.

## Voir aussi
- [Documentation Haskell sur la fonction `Debug.Trace.trace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html#v:trace)
- [Article Medium "Debugging in Haskell with the Debug.Trace Module"](https://medium.com/swlh/debugging-in-haskell-with-the-debug-trace-module-1945665f55d0)
- [Tutoriel vidéo "Debugging with Haskell"](https://www.youtube.com/watch?v=T18YvlEJW2s)