---
title:                "Elm: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà voulu télécharger une page web? Peut-être que vous avez trouvé un article intéressant ou une vidéo amusante et vous souhaitez l'avoir sur votre ordinateur pour y accéder plus tard ou même sans connexion internet. Dans cet article, nous allons explorer comment télécharger une page web en utilisant le langage de programmation Elm.

## Comment faire

Tout d'abord, nous devons importer le module Http qui nous permettra de faire des requêtes HTTP. Ensuite, nous allons créer une fonction qui prendra une URL en paramètre et qui utilisera la fonction `Http.get` pour effectuer la requête. Pour cela, nous allons également utiliser la fonction `expectString` pour convertir la réponse en chaîne de caractères.

```Elm
import Http
import String

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get url expectString
```

Ensuite, nous pouvons appeler cette fonction avec l'URL de la page que nous souhaitons télécharger et la stocker dans un modèle.

```Elm
type alias Model =
    { page : String
    , url : String
    }

init : Model
init =
    { page = ""
    , url = "https://example.com"
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadPage ->
            let
                newModel = { model | page = "" } -- Clear previous page
            in
                ( newModel, fetchPage model.url )

        NewPage page ->
            let
                newModel = { model | page = page }
            in
                ( newModel, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DownloadPage ] [ text "Télécharger" ]
        , div [] [ text model.page ]
        ]
```

Enfin, nous devons gérer la réponse de la requête en créant un nouveau message qui contiendra la page téléchargée. Nous pouvons utiliser la fonction `String.split` pour séparer la réponse en différentes lignes et les afficher dans notre vue.

```Elm
type Msg
    = DownloadPage
    | NewPage String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadPage ->
            let
                newModel = { model | page = "" } -- Clear previous page
            in
                ( newModel, fetchPage model.url )

        NewPage page ->
            let
                lines = String.split "\n" page
                newModel = { model | page = lines }
            in
                ( newModel, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DownloadPage ] [ text "Télécharger" ]
        , div [] (List.map (\line -> div [] [ text line ]) model.page)
        ]
```

## Approfondissement

Télécharger une page web peut sembler simple, mais il y a des choses à prendre en compte pour que cela fonctionne correctement. Par exemple, nous devons nous assurer que l'URL fournie est valide et que nous avons les autorisations nécessaires pour télécharger la page. Il est également important de gérer les erreurs et les réponses de statut HTTP.

## Voir aussi

- [Documentation officielle sur le module Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Exemple complet de téléchargement de page web en Elm](https://github.com/elm-tutorial/examples/blob/master/Http/ParseElmHtml.elm)