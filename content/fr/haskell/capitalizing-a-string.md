---
title:                "Mettre en majuscule une chaîne"
html_title:           "Haskell: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Capitaliser une chaîne signifie transfomer la première lettre de chaque mot en majuscule. Les développeurs le font généralement pour des raisons esthétiques, pour mettre l'accent sur les noms propres, les titres etc.

## Comment faire:

La façon habituelle de capitaliser une chaîne en Haskell est d'utiliser la fonction `toUpper`, fournie par `Data.Char`. Voici un exemple précis:

```Haskell
import Data.Char (toUpper)

capitaliser :: String -> String
capitaliser [] = []
capitaliser (premier:reste) = toUpper premier : map toLower reste
```
Cela transforme la première lettre en majuscule, et garantit que le reste de la phrase est en minuscule.

Exemple de sortie:
 
```Haskell
capitaliser "bonjour, monde!"
"Bonjour, monde!"
```

## Approfondissement:

L'idée de capitaliser les chaînes est assez ancienne dans l'histoire de la programmation. En Haskell, `Data.Char` fournit une gamme de fonctions pour manipuler les caractères. L'utilisation de `toUpper` et de `map toLower` est une méthode courante pour capitaliser les chaînes.

Il existe aussi d'autres façons de capitaliser une chaîne. Par exemple, vous pourriez utiliser une approche récursive pour cela. Cependant, dans les cas courants, le code démontré ci-dessus est suffisant et efficace.

À l'intérieur de la fonction `capitaliser`, la structure `(premier:reste)` est utilisée pour diviser la chaîne d'entrée en son premier caractère (la tête) et le reste de la chaîne (la queue). Le premier caractère est transformé en majuscules tandis que le reste de la chaîne est mis en minuscules.

## Voir aussi:

Les références suivantes sont utiles pour une étude plus approfondie de ce sujet :

- Documentation officielle de Haskell sur Data.Char : http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html
- Un excellent article sur la manipulation de chaînes en Haskell : https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/string