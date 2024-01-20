---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Obtenir la date actuelle dans un programme consiste à récupérer et à utiliser la date et l'heure système. Les développeurs le font pour diverses raisons, dont notamment le suivi des événements, les timestamps et la périodisation des données.

## Comment faire:
Voici comment obtenir la date actuelle en Elm

```Elm
import Time
import Task
import Browser

type alias Model =
    { time : Time.Posix }

init : flags -> ( Model, Cmd Msg )
init _ =
    ( Model Time.millisToPosix 0
    , Task.perform NewTime Time.now
    )

type Msg
    = NewTime Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

main =
    Browser.element
        { init = init
        , update = update
        , view = \_ -> Html.text ""
        , subscriptions = \_ -> Sub.none
        }
```

Cette section de code va obtenir la date et l'heure système.
        
## Approfondissement
Historiquement, Elm n'avait pas de moyen natif d'obtenir la date actuelle, mais avec l'introduction de l'API Time, cette fonctionnalité est désormais disponible.

Une alternative à l'utilisation de `Time.now` serait de faire appel à une API externe ou à un serveur pour obtenir l'heure, bien que ce ne soit pas très pratique ou efficace.

En interne, Elm utilise la fonction `Date.now()` de JavaScript pour obtenir la date et l'heure actuelles, puis il les convertit en un format compatible avec l'architecture Elm.

## À Voir Aussi
[Documentation Elm Time](https://package.elm-lang.org/packages/elm/time/latest/) : Celui-ci est le package officiel Elm qui vous permet d'obtenir le temps POSIX.
[Elm Guide](https://guide.elm-lang.org/) : Le guide officiel d'Elm qui couvre une grande variété de sujets en lien avec ce langage de programmation.