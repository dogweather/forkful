---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi ?
Le téléchargement d'une page Web fait référence au processus de copie des données d'un serveur Web vers un ordinateur local. Les programmeurs le font pour accéder aux données pour diverses raisons, y compris l'analyse, le développement Web, le test, etc.

## Comment faire :
Utilisons le package `elm/http` pour télécharger une page Web. Supposez que nous voulons télécharger la page d'accueil de Google.
```Elm
import Http
import Json.Decode as Decode

main =
  Http.get
    { url = "http://www.google.com"
    , expect = Http.expectStringResponse (\_ -> Decode.succeed)
    }
    |> Http.send HandleResponse
```
Une fois que vous avez reçu la réponse, vous pouvez la gérer comme ceci :
```Elm
type Msg
  = HandleResponse (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HandleResponse (Ok body) ->
      ( { model | content = body }, Cmd.none )

    HandleResponse (Err _) ->
      ( model, Cmd.none )
```
Ici, `HandleResponse` est un message qui gère la réponse du `Http.get`.

## Plongeons plus profondément :
Historiquement, le téléchargement de pages Web était un concept directement lié aux navigateurs. Cependant, avec l'avènement des outils de développement modernes et la popularité croissante des applications single-page, il est devenu une compétence essentielle pour les développeurs.
Quant aux alternatives, beaucoup de gens utilisent également `fetch` ou `axios` en JavaScript pour atteindre le même objectif. Cependant, Elm a sa propre bibliothèque HTTP qui est bien intégrée avec son architecture et favorise l'expérience utilisateur globale en traitant automatiquement les erreurs et en garantissant la fiabilité du code.
Pour plus de détails d'implémentation, veuillez consulter la documentation officielle d'Elm [ici](https://package.elm-lang.org/packages/elm/http/latest/).

## Voir aussi :
1. [Elm Http - Github](https://github.com/elm/http)
2. [Fetching data in Elm](https://korban.net/posts/elm/2018-07-09-basic-http-requests-in-elm/) 
3. [Package manager for Elm](https://package.elm-lang.org/packages/elm/http/latest/Http)