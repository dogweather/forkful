---
title:                "Capitaliser une chaîne de caractères."
html_title:           "Haskell: Capitaliser une chaîne de caractères."
simple_title:         "Capitaliser une chaîne de caractères."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est ce que c'est & Pourquoi?

Capitaliser une chaîne de caractères consiste à mettre la première lettre en majuscule tandis que les autres lettres restent en minuscule. Les programmeurs utilisent cette technique pour améliorer la lisibilité de leur code, notamment pour les noms de variables ou les messages affichés à l'utilisateur.

## Comment faire:

Voici un exemple de code en Haskell qui utilise la fonction `toUpper` pour capitaliser une chaîne de caractères:

```Haskell
import Data.Char
capitalize :: String -> String
capitalize str = toUpper (head str) : tail (map toLower str)
```

Si nous appliquons cette fonction à la chaîne `"hello"`, nous obtiendrons `"Hello"` en sortie. Nous pouvons également utiliser la fonction `words` pour capitaliser chaque mot d'une phrase:

```Haskell
capsWords :: String -> String
capsWords str = unwords (map capitalize (words str))
```

Ainsi, si nous appliquons `capsWords` à la phrase `"hello world"`, nous obtiendrons `"Hello World"` en sortie.

## Plongée en profondeur:

L'utilisation de la capitalisation dans le développement logiciel remonte aux premiers langages de programmation tels que le COBOL et le Fortran. Cependant, cela a été popularisé avec l'émergence de langages fonctionnels comme Haskell qui mettent l'accent sur la manipulation de données immuables.

Il existe également d'autres méthodes pour capitaliser une chaîne de caractères en fonction de certains critères comme la première lettre de chaque mot ou la première lettre après un espace.

En termes d'implémentation, la fonction `toUpper` utilise la table ASCII pour convertir une lettre en minuscule en majuscule en ajoutant ou en soustrayant la constante correspondant à la différence entre les valeurs des lettres.

## Voir aussi:

Pour en savoir plus sur les fonctions de manipulation de chaînes de caractères en Haskell, consultez la [documentation officielle](https://hackage.haskell.org/package/base/docs/Prelude.html#t:String). Vous pouvez également découvrir d'autres méthodes de capitalisation en explorant la [bibliothèque de fonctions de manipulation de caractères](https://hackage.haskell.org/package/base/docs/Data-Char.html).