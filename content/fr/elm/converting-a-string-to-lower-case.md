---
title:                "Elm: Conversion d'une chaîne en minuscules"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm débutant, vous vous demandez peut-être pourquoi devriez-vous vous soucier de convertir une chaîne en minuscules. Eh bien, il y a plusieurs raisons pour lesquelles vous pourriez avoir besoin de le faire. Par exemple, vous pourriez avoir besoin de normaliser une entrée utilisateur ou de comparer des chaînes de caractères sans tenir compte de la casse. Dans cet article, nous allons voir comment convertir efficacement une chaîne en minuscules en utilisant Elm.

## Comment faire

Convertir une chaîne en minuscules est un processus simple en Elm. Tout ce que vous avez à faire est d'utiliser la fonction `String.toLower` qui prend une chaîne en entrée et renvoie une nouvelle chaîne en minuscules. Voici un exemple de code:

```elm
nom = "EMMA"
nomEnMinuscules = String.toLower nom

```
La variable `nom` contient la chaîne "EMMA" et la variable `nomEnMinuscules` contient la chaîne "emma" après l'exécution de la `String.toLower` fonction.

Si vous voulez convertir une chaîne en minuscules à mesure que l'utilisateur tape, vous pouvez utiliser la fonction `onInput` pour déclencher la conversion à chaque fois qu'une nouvelle valeur est saisie dans un champ de saisie. Voici un exemple de code:

```elm
-- Définition du modèle avec un champ de saisie pour le nom
type alias Model =
    { nom : String }

-- Convertir la chaîne en minuscules à chaque saisie d'utilisateur
view : Model -> Html Msg
view model =
    input [ onInput (\newVal -> { nom = String.toLower newVal }) ] []

```

## Deep Dive

Si vous souhaitez comprendre en profondeur comment la fonction `String.toLower` fonctionne en Elm, voici quelques informations pour vous. En interne, cette fonction utilise la fonction`Char.toLower` pour convertir chaque caractère de la chaîne en minuscules. Cela signifie que cette fonction prend en charge tous les caractères ASCII, Unicode et les caractères en majuscules et les convertit en minuscules.

Un point intéressant à noter est que la fonction `Char.toLower` ne modifie pas les caractères qui n'ont pas de forme alternative. Par exemple, la lettre "k" en minuscules reste "k" en minuscules, car elle n'a pas de forme alternative en majuscules. Cela permet d'assurer que la fonction de conversion reste consistante et prévisible pour toutes les chaînes.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes en Elm, vous pouvez consulter les ressources suivantes:

- [Documentation de la bibliothèque Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Blog officiel d'Elm](https://elm-lang.org/)
- [Elm for Beginners - Une introduction à la programmation Elm](https://elmprogramming.com/)

Merci d'avoir lu cet article sur la conversion de chaînes en minuscules en utilisant Elm. Nous espérons que cela vous aidera dans votre voyage de programmation en Elm !