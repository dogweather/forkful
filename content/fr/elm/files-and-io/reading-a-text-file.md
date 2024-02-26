---
date: 2024-01-20 17:54:43.380497-07:00
description: "Lire un fichier texte, c'est r\xE9cup\xE9rer les donn\xE9es \xE9crites\
  \ dedans. On le fait pour acc\xE9der et manipuler ces donn\xE9es, comme charger\
  \ des configurations,\u2026"
lastmod: '2024-02-25T18:49:54.448494-07:00'
model: gpt-4-1106-preview
summary: "Lire un fichier texte, c'est r\xE9cup\xE9rer les donn\xE9es \xE9crites dedans.\
  \ On le fait pour acc\xE9der et manipuler ces donn\xE9es, comme charger des configurations,\u2026"
title: Lecture d'un fichier texte
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire un fichier texte, c'est récupérer les données écrites dedans. On le fait pour accéder et manipuler ces données, comme charger des configurations, importer des données utilisateurs ou analyser des contenus.

## Comment faire :
Elm fonctionne sur le web, alors pour lire un fichier texte, on interagit avec des APIs de navigateur. Voici un exemple avec `File.Selector` et `File.Reader`.

```Elm
module Main exposing (..)

import Browser
import File exposing (File)
import File.Selector as Selector
import File.Reader as Reader
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model =
    { fileContent : Maybe String }


initialModel : Model
initialModel =
    { fileContent = Nothing }


type Msg
    = SelectFile
    | FileSelected File
    | FileRead (Result String String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SelectFile ->
            ( model, Selector.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( model, Reader.readAsText file FileRead )

        FileRead (Ok content) ->
            ( { model | fileContent = Just content }, Cmd.none )

        FileRead (Err _) ->
            ( { model | fileContent = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Sélectionner un fichier" ]
        , case model.fileContent of
            Just content ->
                div [] [ text content ]

            Nothing ->
                div [] [ text "Pas de contenu ou erreur de lecture." ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
```

Sortie typique :
- Si le lecteur charge un fichier : On affiche le contenu du fichier.
- En cas d'erreur : "Pas de contenu ou erreur de lecture."

## Décryptage
Historiquement, Elm a été conçu pour des applications web sûres: on ne peut pas simplement lire un fichier du système de fichiers local. À la place, Elm utilise des APIs JavaScript comme intermédiaires. Avant Elm 0.19, on utilisait les ports pour gérer l'interopérabilité côté JavaScript, tandis que maintenant, Elm offre `File` et des modules associés pour simplifier ces opérations.

Les alternatives comprennent l'enregistrement des données côté serveur ou l'utilisation de technologies côté client comme IndexedDB pour les applications plus complexes. Pour la lecture simple, l'API FileReader fournie par les navigateurs web est souvent suffisante.

Côté implémentation, Elm utilise des commandes (`Cmd Msg`) pour gérer les effets secondaires tels que la lecture de fichiers, ce qui maintient la pureté de ses fonctions et garantit des applications fiables.

## Voir aussi
- Documentation Elm sur les fichiers : [https://package.elm-lang.org/packages/elm/file/latest/](https://package.elm-lang.org/packages/elm/file/latest/)
- Guide Elm sur les ports (interactions Elm-JavaScript) : [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- Documentation MDN sur l'API FileReader : [https://developer.mozilla.org/fr/docs/Web/API/FileReader](https://developer.mozilla.org/fr/docs/Web/API/FileReader)
