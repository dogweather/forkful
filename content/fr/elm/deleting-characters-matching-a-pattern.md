---
title:                "Elm: Supprimer les caractères correspondants à un motif"
simple_title:         "Supprimer les caractères correspondants à un motif"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut sembler une tâche fastidieuse, mais cela peut être très utile dans certaines situations. Par exemple, si vous avez un grand bloc de texte et que vous voulez supprimer toutes les lettres minuscules, cela peut être fait en utilisant une fonction de suppression de caractères. Dans cet article, nous allons explorer comment le faire en utilisant le langage de programmation Elm.

## Comment faire

Tout d'abord, nous devons définir une fonction qui prendra en entrée une chaîne de caractères et un motif, et renverra une nouvelle chaîne sans les caractères correspondants au modèle spécifié. En Elm, cela peut être fait en utilisant la fonction native "String.filter". Voici un exemple de code :

```Elm
module Main exposing (main)

import Html exposing (text)

removePattern : String -> String -> String
removePattern str pattern =
    String.filter (\c -> not (String.contains pattern c)) str

main : Html msg
main =
    text (removePattern "Bonjour" "aeiou") -- Renvoie "Bnjr"
```

Dans cet exemple, nous importons le module Html pour pouvoir afficher le résultat de notre fonction "removePattern" sous forme de texte. Ensuite, nous définissons la fonction en utilisant "String.filter" en passant une fonction anonyme qui vérifie si le caractère n'est pas présent dans le modèle spécifié. Enfin, nous appelons la fonction en lui passant une chaîne de caractères et un motif et affichons le résultat.

## Plongée en profondeur

Maintenant que nous avons vu comment supprimer des caractères correspondant à un motif de manière simple, il est utile de comprendre comment la fonction "String.filter" fonctionne en interne. Elle prend en paramètre une fonction qui vérifie chaque caractère de la chaîne de caractères et renvoie "True" si le caractère doit être inclus dans la nouvelle chaîne et "False" si le caractère doit être exclu. Cette fonction peut être écrite de différentes manières selon vos besoins.

## Voir aussi

- Documentation officielle sur les chaînes de caractères en Elm : https://guide.elm-lang.org/strings/
- Tutoriel sur les fonctions de manipulation des chaînes de caractères en Elm : https://www.elm-tutorial.org/en/02-elm-arch/05-strings.html
- Exemples pratiques d'utilisation de "String.filter" en Elm : https://gist.github.com/rgrempel/d707b9ff05ef9146096b9e6993ab010b