---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "Elm: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Pourquoi

Si vous êtes un programmeur Elm, il est probable que vous ayez souvent besoin de convertir des chaînes de caractères (strings) en lettres minuscules (lower case) pour différentes raisons. Cela peut être utile pour comparer des chaînes, rechercher des mots-clés ou simplement pour uniformiser le formatage des données. Heureusement, Elm a une fonction intégrée qui facilite cette tâche !

##Comment Faire

Pour convertir une chaîne de caractères en lettres minuscules en Elm, utilisez simplement la fonction `String.toLower` :

```Elm
phrase = "Bonjour MONDE"
nouvellePhrase = String.toLower phrase
```

La variable `nouvellePhrase` contiendra la chaîne de caractères "bonjour monde". Vous pouvez également utiliser cette fonction directement dans une chaîne interpolée en utilisant la syntaxe de l'opérateur `$` :

```Elm
phrase = "Bienvenue dans le $."
nouvellePhrase = "État MONDE" |> String.toLower |> String.append phrase
```

Dans cet exemple, la variable `nouvellePhrase` contiendra la chaîne de caractères "bienvenue dans le monde".

##Plongée en Profondeur

Si vous souhaitez en savoir plus sur la façon dont la fonction `String.toLower` fonctionne en interne, voici quelques détails. Tout d'abord, il est important de noter que cette fonction est définie dans le module `String` d'Elm, il vous suffit donc de l'importer dans votre code pour l'utiliser.

Ensuite, la fonction elle-même utilise la fonction `String.foldl` pour parcourir chaque caractère de la chaîne de caractères et applique la fonction `Char.toLower` à chacun d'entre eux. Enfin, elle utilise la fonction `String.fromList` pour reconstruire une nouvelle chaîne de caractères à partir des caractères convertis.

Plus de détails techniques peuvent être trouvés dans la documentation officielle d'Elm sur les chaînes de caractères et les fonctions utilisées.

##Voir Aussi

- La documentation sur les chaînes de caractères en Elm : http://package.elm-lang.org/packages/elm-lang/core/latest/String
- Un guide complet sur les fonctions intégrées en Elm : http://package.elm-lang.org/packages/elm-lang/core/latest/Basics