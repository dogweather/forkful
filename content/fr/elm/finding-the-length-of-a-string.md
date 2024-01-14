---
title:    "Elm: Trouver la longueur d'une chaîne de caractères"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm, vous avez probablement rencontré des situations où vous devez connaître la longueur d'une chaîne de caractères. Que ce soit pour valider une entrée utilisateur ou pour manipuler des données, il est crucial de savoir comment trouver la longueur d'une chaîne. Dans cet article, nous allons explorer différentes façons de le faire en Elm.

## Comment faire

Il existe plusieurs façons de trouver la longueur d'une chaîne en Elm. Voici quelques exemples de code pour vous montrer comment faire :

```elm
-- Utiliser la fonction String.length
String.length "Bonjour" -- Output: 7

-- Utiliser une liste de caractères et utiliser la fonction List.length
List.length (String.toList "Bonjour") -- Output: 7
```

Comme vous pouvez le voir, la fonction `String.length` donne directement la longueur d'une chaîne, tandis que la méthode avec `List.length` nécessite d'abord de convertir la chaîne en une liste de caractères.

## Plongée en profondeur

Maintenant que nous avons vu comment trouver la longueur d'une chaîne en Elm, explorons un peu plus en profondeur pour mieux comprendre comment cela fonctionne. En réalité, la fonction `String.length` utilise en interne la fonction `List.length` en convertissant la chaîne en une liste de caractères avant de trouver sa longueur. Cela peut sembler compliqué, mais c'est en fait une optimisation qui rend la fonction plus efficace.

De plus, il est important de noter que la longueur d'une chaîne est différente de la taille de la chaîne. La longueur se réfère au nombre de caractères, tandis que la taille se réfère au nombre d'octets. Par conséquent, la longueur d'une chaîne peut être différente de sa taille, en particulier lorsque des caractères multibytes sont utilisés.

## Voir aussi

- [Documentation officielle Elm sur les chaînes](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Guide pratique pour les débutants en Elm](https://guide.elm-lang.org/) 
- [Vidéo de présentation d'Elm (en français)](https://www.youtube.com/watch?v=bbUu-VJR-Sw)

Maintenant que vous maîtrisez la façon de trouver la longueur d'une chaîne en Elm, vous pouvez l'appliquer à vos propres projets. N'hésitez pas à explorer davantage les différentes fonctions de manipulation de chaînes en Elm pour améliorer vos compétences en programmation. Bonne chance !