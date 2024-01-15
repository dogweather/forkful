---
title:                "La lecture des arguments de ligne de commande"
html_title:           "Elm: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Elm, vous avez probablement déjà été confronté à la nécessité de lire des arguments de ligne de commande dans votre code. Cela peut sembler intimidant au premier abord, mais une fois que vous savez comment le faire, c'est un savoir-faire utile à avoir dans votre boîte à outils de programmation.

## Comment faire

Pour lire les arguments de ligne de commande en Elm, vous pouvez utiliser la bibliothèque [elm-tools/parser](https://package.elm-lang.org/packages/elm-tools/parser/latest/). Voici un exemple simple de code qui lit un argument appelé "nom" et affiche une chaîne de caractères avec le nom fourni:

```Elm
import Parser exposing (..)
import Tasks exposing (..)

main : Program ()
main =
   Parser.map display 
      (succeed print <|
         string "nom" `andThen` 
         map concat getChompedString)
   |> parse Process.arguments
   |> andThen (\x -> performTask x [])

display : String -> Task x ()
display name =
   putStrLn ("Bonjour" ++ name ++ "!")


```

Si vous exécutez ce code avec la commande `elm make lire-arguments.elm --nom=Benoit`, vous devriez voir "Bonjour Benoit!" imprimé dans votre terminal.

## Plongée en profondeur

La bibliothèque elm-tools/parser vous offre une grande flexibilité dans la façon dont vous lisez les arguments de ligne de commande. Vous pouvez utiliser des combinateurs pour créer des parsers qui correspondent à des expressions régulières ou utiliser des parsers déjà existants pour lire des types de données tels que des nombres ou des booléens. N'hésitez pas à explorer la documentation pour découvrir toutes les possibilités offertes.

## Voir aussi

- [Documentation de elm-tools/parser](https://package.elm-lang.org/packages/elm-tools/parser/latest/)
- [Exemple complet de lecture d'arguments de ligne de commande avec elm-tools/parser](https://github.com/elm/library/blob/master/src/Parser.elm)
- [Guide sur la manipulation des entrées utilisateur en Elm](https://guide.elm-lang.org/interop/user_input.html)