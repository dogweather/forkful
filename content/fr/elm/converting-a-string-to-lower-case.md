---
title:                "Elm: Convertissement d'une chaîne de caractères en minuscules"
simple_title:         "Convertissement d'une chaîne de caractères en minuscules"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous avez peut-être déjà rencontré le besoin de convertir une chaîne de caractères en minuscules. Peut-être que vous avez un champ de recherche sur votre site et que vous voulez vous assurer que les entrées des utilisateurs sont toutes en minuscules pour faciliter la recherche. Ou peut-être que vous travaillez avec des données sensibles et que vous devez les convertir en minuscules pour les sécuriser. Dans cet article, nous allons nous plonger dans le processus de conversion d'une chaîne de caractères en minuscules en utilisant Elm.

## Comment faire

Tout d'abord, nous allons créer une fonction dans notre code Elm qui prendra en entrée une chaîne de caractères et la convertira en minuscules. Nous allons appeler cette fonction `toLowercase` et lui donner un paramètre `string` de type `String`. Voici à quoi cela ressemblerait dans notre code :

```Elm
toLowercase : String -> String
toLowercase string =
    String.toLower string
```

Dans cet exemple, nous utilisons la fonction `toLower` fournie par le module `String` d'Elm qui convertit une chaîne de caractères en minuscules. Notre fonction `toLowercase` peut maintenant être utilisée pour convertir n'importe quelle chaîne de caractères en minuscules. Voici un exemple de code utilisant notre fonction :

```Elm
myString = "Bonjour Elm"
convertedString = toLowercase myString

Debug.log "Résultat :" convertedString
```

La sortie de ce code serait `"bonjour elm"`.

## Plongée en profondeur

Maintenant que nous savons comment convertir une chaîne de caractères en minuscules en utilisant Elm, examinons de plus près ce qui se passe en arrière-plan. La fonction `toLower` utilise en réalité une bibliothèque externe appelée "ASCII": elle prend en charge la conversion des caractères ASCII en minuscules. Ce que cela signifie, c'est que si vous avez des caractères spéciaux ou des lettres accentuées dans votre chaîne de caractères, ils ne seront pas convertis en minuscules. Par exemple, si nous utilisons notre fonction `toLowercase` sur la chaîne `"À bientôt"`, la sortie sera `"À bientôt"` sans aucune modification. Cela peut être important à prendre en compte selon l'utilisation que vous faites de cette fonction dans votre code.

## Voir aussi

- [Documentation Elm pour la fonction toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Documentation sur la bibliothèque "ASCII"](https://package.elm-lang.org/packages/elm/core/latest/Char#toLower)