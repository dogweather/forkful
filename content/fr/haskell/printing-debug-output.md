---
title:    "Haskell: Imprimer la sortie de débogage"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imprimer des sorties de débogage est une pratique très utile lors du développement en Haskell. Cela permet de vérifier les valeurs des variables et de détecter les erreurs dans le code. Dans cet article, nous allons découvrir pourquoi utiliser cette technique, comment le faire et nous ferons une plongée plus profonde dans le sujet.

## Comment faire

Pour imprimer des sorties de débogage en Haskell, il suffit d'utiliser la fonction `print`. Voici un exemple de code :

```
main = do
  let x = 10
  let y = 20
  print x
  print y
```

La sortie de ce code sera :

```
10
20
```

Comme vous pouvez le voir, la fonction `print` affiche simplement la valeur de la variable passée en argument. Cela peut être très utile pour vérifier si les valeurs sont correctes à un certain point du programme.

## Plongée profonde

Il est également possible d'utiliser la fonction `putStrLn` pour imprimer des chaînes de caractères en plus des valeurs de variables. Voici un exemple :

```
main = do
  let x = 10
  putStrLn "La valeur de x est :"
  print x
```

La sortie sera :

```
La valeur de x est :
10
```

De plus, il est possible de formater la sortie à l'aide de la fonction `printf` provenant du module `Text.Printf`. Voici un exemple :

```
import Text.Printf

main = do
  let x = 10
  let y = 20
  printf "La somme de %d et %d est : %d" x y (x + y)
```

La sortie sera :

```
La somme de 10 et 20 est : 30
```

En utilisant ces différentes fonctions, vous pouvez facilement afficher des informations précises dans vos programmes et détecter les erreurs plus rapidement.

## Voir aussi

- [Documentation officielle de Haskell](https://www.haskell.org/documentation/)
- [Tutoriel de Haskell pour débutants](https://www.haskell.org/tutorial/)