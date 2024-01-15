---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Elm: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous êtes sûrement demandé plusieurs fois comment trouver la longueur d'une chaîne de caractères en Elm. Peut-être avez-vous besoin de cette information pour vérifier si le texte entré par l'utilisateur est suffisamment long, ou pour effectuer une opération mathématique complexe. Quelle que soit la raison, savoir comment obtenir la longueur d'une chaîne de caractères vous permet de résoudre un large éventail de problèmes en programmation.

## Comment faire

Il existe plusieurs façons de trouver la longueur d'une chaîne de caractères en Elm. Voici deux méthodes couramment utilisées :

1. La fonction `String.length` : cette fonction prend en paramètre une chaîne de caractères et renvoie un entier représentant sa longueur.

```Elm
String.length "Bonjour" -- renvoie 7
```

2. Utiliser la méthode `length` sur une liste de caractères : cela peut sembler un peu plus compliqué, mais en réalité, cela fonctionne exactement comme la méthode précédente.

```Elm
length <| String.toList "Bonjour" -- renvoie 7
```

Vous pouvez également utiliser des fonctions telles que `List.length` et `Array.length` pour obtenir la longueur d'une liste ou d'un tableau de caractères.

## Plongée en profondeur

Il est intéressant de noter que la fonction `String.length` utilise la méthode `length` sur la liste de caractères sous-jacente pour obtenir la longueur de la chaîne de caractères. Cela signifie que l'utilisation de `String.toList` pour convertir la chaîne de caractères en une liste de caractères peut sembler redondante, mais c'est en fait plus efficace car `String.toList` utilise un algorithme optimisé pour récupérer les caractères de la chaîne de caractères.

Il est important de noter que la longueur d'une chaîne de caractères en Elm correspond au nombre de caractères qu'elle contient, et non au nombre d'octets. Cela peut sembler évident, mais lorsque vous travaillez avec des langues telles que le chinois ou le japonais, où un seul caractère peut correspondre à plusieurs octets, il est important de garder cela à l'esprit.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes de caractères en Elm, vous pouvez consulter ces ressources :

- [Guide de la documentation officielle Elm sur les chaînes de caractères](https://guide.elm-lang.org/strings/)
- [Fonctions utiles pour travailler avec des chaînes de caractères en Elm](https://package.elm-lang.org/packages/elm/core/latest/String)

Maintenant que vous savez comment obtenir la longueur d'une chaîne de caractères en Elm, vous pouvez l'appliquer à vos projets et découvrir de nouvelles façons d'utiliser cette fonctionnalité pratique. Bonne programmation !