---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
L'interpolation de chaînes est une méthode qui permet d'insérer des valeurs dans une chaîne de caractères. Nous le faisons pour créer des chaînes dynamiques sans avoir à concaténer manuellement les valeurs.

## Comment faire :
Dans Elm, nous interpolons une chaîne en utilisant la fonction `++`. Regardez cet exemple :
```Elm
let
    nom = "Jean"
    message = "Bonjour " ++ nom ++ ", comment ça va?"
in
    message
```
Sortie :
```Elm
"Bonjour Jean, comment ça va?"
```
Dans cet exemple, nous avons interpolé la variable `nom` dans la chaîne `message`.

## Analyse approfondie
L'interpolation de chaînes est une fonctionnalité courante dans de nombreux langages de programmation. Bien que Elm n'ait pas d'interpolation de chaîne intégrée comme ES6 ou Python, l'opérateur `++` fournit une alternative simple et claire. Cependant, notez que cette fonction à une compléxité temporelle linéaire, ce qui signifie qu'elle peut devenir lente pour de très longues chaînes.

## Voir aussi
Pour plus d'informations sur l'interpolation de chaînes et ses alternatives, consultez ces liens :
1. [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String) 
2. [Discussion Elm sur String Concatenation vs Interpolation](https://discourse.elm-lang.org/t/string-concatenation-vs-interpolation/310)
3. [Performance de String.join vs ++](https://www.brianthicks.com/post/2018/03/27/concatenate-your-strings-with-string-join/)