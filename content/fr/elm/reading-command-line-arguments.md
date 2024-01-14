---
title:                "Elm: La lecture des arguments en ligne de commande"
simple_title:         "La lecture des arguments en ligne de commande"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Chaque développeur a probablement rencontré le besoin de lire des arguments de ligne de commande à un moment donné dans son parcours de programmation. Cela peut sembler intimidant au premier abord, mais heureusement, Elm fournit une solution simple et efficace pour relever ce défi.

## Comment faire

Pour lire des arguments de ligne de commande en Elm, nous allons utiliser la fonction `getArgs` du module `Platform.Cmd` ainsi que le module `Elm-Kernel` pour accéder à l'interface JavaScript. Voici un exemple de code utilisant ces deux éléments :

```Elm
import Platform.Cmd exposing (Cmd)
import Html exposing (text)
import Elm.Kernel.Platform exposing (load)

main : Program () String ()
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( String, Cmd () )
init _ =
    ( "", Platform.Cmd.getArgs )


update : () -> String -> ( String, Cmd () )
update _ arg =
    ( "Les arguments de ligne de commande sont : " ++ arg, Cmd.none )


subscriptions : () -> Sub () 
subscriptions _ =
    Sub.none


view : String -> Html Msg 
view arg =
    text arg
```

Dans cet exemple, nous utilisons la fonction `getArgs` dans notre fonction `init` pour récupérer les arguments de ligne de commande et les stocker dans notre modèle. Ensuite, dans notre fonction `update`, nous pouvons utiliser ces arguments pour mettre à jour notre modèle ou effectuer toute autre action souhaitée.

## Plongée en profondeur

Maintenant que nous avons vu comment utiliser la fonction `getArgs`, plongeons un peu plus en profondeur pour mieux comprendre comment elle fonctionne. En réalité, cette fonction utilise une API JavaScript sous-jacente pour récupérer les arguments de ligne de commande de l'application et les renvoyer sous forme de chaîne de caractères. Cela signifie que pour lire des arguments de ligne de commande en Elm, nous devons également avoir un script JavaScript lié à notre application.

Pour ce faire, nous allons utiliser le module `Elm-Kernel` pour accéder à l'interface JavaScript et appeler la fonction `load` pour charger un script externe. Ce script contiendra notre fonction JavaScript pour récupérer les arguments de ligne de commande et les renvoyer à notre application Elm.

## Voir aussi

Pour en savoir plus sur la lecture des arguments de ligne de commande en Elm, vous pouvez consulter la documentation officielle d'Elm ainsi que d'autres articles en ligne tels que :

- [Documentation officielle d'Elm](https://guide.elm-lang.org)
- [Article de blog sur la lecture des arguments de ligne de commande en Elm](https://www.dailydrip.com/blog/elm-argument-parsing)
- [Exemple de projet GitHub utilisant la lecture des arguments de ligne de commande en Elm](https://github.com/elm/projects/tree/master/command-line-args)

Maintenant que vous êtes familiarisé avec la lecture des commandes en ligne en Elm, vous pouvez l'incorporer dans vos projets et tirer parti de cette fonctionnalité pratique et essentielle.