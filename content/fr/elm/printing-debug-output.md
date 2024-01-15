---
title:                "Affichage des sorties de débogage"
html_title:           "Elm: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous avez sûrement rencontré des bugs dans votre code. Pour les résoudre efficacement, il est souvent nécessaire d'afficher des informations de débogage, c'est-à-dire des informations supplémentaires sur l'état de votre programme à un moment donné. Dans cet article, nous allons plonger dans la méthode recommandée pour afficher ces informations de débogage en utilisant la fonction `Debug.log`.

## Comment faire

Pour utiliser `Debug.log`, vous devez d'abord l'importer dans votre fichier Elm. Ensuite, vous pouvez l'utiliser en lui passant deux arguments : une chaîne de caractères et une valeur que vous souhaitez afficher. Par exemple :

```
import Debug

myValue = 42

Debug.log "Ma valeur est :" myValue
--> Affiche "Ma valeur est : 42" dans la console de votre navigateur
```

Vous pouvez également utiliser `Debug.log` pour afficher le contenu d'une variable ou d'une expression plus complexe à l'intérieur d'une fonction, ce qui peut être utile pour comprendre ce qui se passe dans votre code. Par exemple :

```
import Debug

myList = [1, 2, 3, 4, 5]

squareList list = 
  List.map (\x -> Debug.log "Valeur à l'intérieur de la fonction :" x * x) list

squareList myList
--> Affiche "Valeur à l'intérieur de la fonction : 1", "Valeur à l'intérieur de la fonction : 4", etc. dans la console de votre navigateur
--> Retourne [1, 4, 9, 16, 25] comme résultat de la fonction
```

## Plongée en profondeur

Il y a plusieurs choses à garder à l'esprit lorsque vous utilisez `Debug.log`. Tout d'abord, il s'agit d'une fonction de débogage et ne doit pas être utilisée dans votre code de production final, car elle peut considérablement ralentir votre application.

Deuxièmement, il est important d'utiliser `Debug.log` de manière sélective et de ne pas en abuser. Trop d'informations de débogage peuvent rendre votre code difficile à lire et à suivre.

Enfin, gardez à l'esprit que les informations de débogage s'affichent dans la console de votre navigateur et peuvent donc être vues par les utilisateurs de votre application. Veillez à ne pas afficher d'informations sensibles ou confidentielles avec `Debug.log`.

## Voir aussi

- [La documentation officielle sur Debug.log](https://package.elm-lang.org/packages/elm/core/latest/Debug#log)
- [Cet article en anglais pour plus d'informations sur l'utilisation de Debug.log](https://thoughtbot.com/blog/debugging-with-debug-log-in-elm)

Maintenant que vous connaissez la meilleure méthode pour afficher des informations de débogage dans votre code Elm, vous pouvez facilement résoudre les bugs et rendre votre code plus robuste. N'oubliez pas de ne pas trop en abuser et de ne pas les afficher dans votre code de production final !