---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'envoi d'une requête HTTP est un moyen de demander des données à un serveur. Les programmeurs le font pour communiquer avec des serveurs web, pour obtenir ou envoyer des données.

## Comment faire:

Ici, nous utilisons la bibliothèque `Http` dans Elm pour envoyer une requête GET. 

```Elm
import Http
import Json.Decode as Decode

type alias User = 
    { id : Int
    , name : String
    }

getUser: Int -> Cmd Msg
getUser userId =
    Http.get 
        { url = "https://jsonplaceholder.typicode.com/users/" ++ String.fromInt userId
        , expect = Http.expectJson GotUser (Decode.field "name" Decode.string)
        }

type Msg = 
    GotUser (Result Http.Error String)
```

La réponse du serveur sera traitée dans la fonction `update`.

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUser result ->
            case result of
                Ok user ->
                    ( { model | user = Just user.name }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
```

Dans l'exemple, `GotUser` est un message qui génère une nouvelle commande à partir du `Result` renvoyé par l'API.

## Plongée en profondeur

Historiquement, XMLHttpRequest était la première méthode pour envoyer des requêtes HTTP dans le navigateur. Elm utilise une enveloppe autour de XMLHttpRequest appelée `elm/http`. 

Il existe des alternatives à l'envoi de requêtes HTTP telles que GraphQL, WebSockets qui ont différentes caractéristiques de performance et d'utilisation.

En Elm, l'envoi d'une requête HTTP est effectué de manière asynchrone. Cela signifie que votre application ne sera pas bloquée pendant l'envoi de la requête. Au lieu de cela, vous créez une `Cmd` qui est ensuite exécutée par le runtime Elm, et une fois que la réponse est prête, une fonction de mise à jour sera appelée avec le résultat.

## Voir Aussi

1. [Documentation officielle Elm](https://package.elm-lang.org/packages/elm/http/latest/Http): Pour plus de détails sur l'utilisation des requêtes HTTP avec Elm.
2. [Explication détaillée sur les requêtes HTTP](https://guide.elm-lang.org/effects/http.html): Un guide par étapes sur la façon de gérer les requêtes HTTP en Elm.
3. [API JSONPlaceholder](https://jsonplaceholder.typicode.com/): Une fausse API en ligne pour tester la récupération de données.
4. [Json.Decode - Elm](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode): Pour en savoir plus sur la décodification des réponses JSON en Elm. 

Bon codage!