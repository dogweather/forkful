---
title:                "Imprimer la sortie de débogage"
html_title:           "Haskell: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a souvent des moments où vous souhaitez vérifier et comprendre ce qui se passe dans votre code Haskell pendant son exécution. Imprimer des messages de débogage peut vous aider à suivre le flux de votre programme et à détecter les erreurs ou les bogues plus facilement.

## Comment faire

Pour imprimer des messages de débogage en Haskell, vous pouvez utiliser la fonction `print`. Elle accepte n'importe quelle expression en entrée et la convertit en chaîne de caractères avant de l'imprimer dans la console. Par exemple :

```Haskell
a <- 10
b <- 5
print (a + b)
```

Cela affichera `15` dans la console. Vous pouvez également utiliser la fonction `putStrLn` pour imprimer des chaînes de caractères, comme dans l'exemple suivant :

```Haskell
putStrLn "Le résultat est:"
print (a + b)
```

Cela affichera :

```
Le résultat est:
15
```

## Plongée en profondeur

Si vous avez besoin d'imprimer des messages de débogage complexes ou d'accéder à des variables dans plusieurs parties de votre code, vous pouvez utiliser la bibliothèque `Debug.Trace`. Elle fournit plusieurs fonctions utiles pour l'impression de débogage, notamment `trace`, `traceM` et `traceIO`.

```Haskell
import Debug.Trace

myFunction :: Int -> Int
myFunction x = trace ("Valeur de x : " ++ show x) (x * 2)

main = do
  let a = myFunction 5
  print a
```

Cela affichera :

```
Valeur de x : 5
10
```

Attention cependant, ces fonctions de débogage ne doivent être utilisées que pendant le développement et ne doivent pas être présentes dans votre code final, car elles peuvent avoir des effets indésirables sur les performances.

## Voir aussi

- [Documentation de la fonction `print`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:print)
- [Documentation de la bibliothèque `Debug.Trace`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Debug-Trace.html)
- [Guide de débogage en Haskell](https://wiki.haskell.org/Debugging)