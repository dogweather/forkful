---
title:    "Elm: Conversion d'une chaîne en minuscules"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de vous demander pourquoi vous devriez vous préoccuper de convertir une chaîne en minuscules en Elm. Eh bien, la réponse est simple : la conversion en minuscules est un élément essentiel du traitement des données textuelles. Que vous travailliez sur un projet de traitement de texte ou que vous ayez besoin de comparer des chaînes de caractères sans tenir compte de la casse, la conversion en minuscules est une compétence de base que tout développeur Elm devrait maîtriser.

## Comment faire

La conversion d'une chaîne en minuscules en Elm est très simple. Tout ce que vous avez à faire est d'utiliser la fonction `String.toLower` en lui passant la chaîne que vous souhaitez convertir en minuscules. Voyons un exemple concret :

```Elm
let
    string = "ELM PROGRAMMING"
in
    String.toLower string
```

Lorsque vous exécuterez ce code, vous obtiendrez la sortie suivante :

```Elm
"elm programming"
```

Comme vous pouvez le constater, la fonction `String.toLower` a correctement converti la chaîne en minuscules.

## Plongée en profondeur

Maintenant que vous savez comment convertir une chaîne en minuscules en Elm, il est important de comprendre comment cette fonction fonctionne en interne. Fondamentalement, la fonction parcourt chaque caractère de la chaîne et remplace les lettres majuscules par leurs équivalents en minuscules, en utilisant le code ASCII comme référence. Elle renvoie ensuite la chaîne convertie en minuscules.

Il est également important de noter que la fonction `String.toLower` ne modifie pas la chaîne d'origine, mais renvoie plutôt une nouvelle chaîne en minuscules. Cela est dû au fait que les chaînes sont des valeurs immuables en Elm, c'est-à-dire qu'elles ne peuvent pas être modifiées une fois qu'elles ont été créées.

## Voir aussi

- [Documentation officielle sur la fonction `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Exemples pratiques pour utiliser la fonction `String.toLower`](https://elmprogramming.com/convert-string-to-lowercase-elm)

Maintenant que vous savez comment convertir une chaîne en minuscules en Elm, vous pouvez l'utiliser dans vos projets pour gérer facilement les données textuelles. La compréhension de cette fonctionnalité fondamentale vous aidera à devenir un meilleur développeur Elm. Bonne programmation !