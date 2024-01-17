---
title:                "Envoyer une requête HTTP avec une authentification basique"
html_title:           "Elm: Envoyer une requête HTTP avec une authentification basique"
simple_title:         "Envoyer une requête HTTP avec une authentification basique"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Quoi et pourquoi?
Envoyer une requête HTTP avec une authentification de base est un moyen pour les programmeurs d'accéder à des ressources protégées sur le Web. Cela leur permet d'autoriser l'accès à des utilisateurs spécifiques en vérifiant leurs identifiants.

# Comment faire:
Voici un exemple de code en Elm pour envoyer une requête HTTP avec une authentification de base. Le code suivant utilise une bibliothèque HTTP appelée `elm/http` pour effectuer la requête:

```Elm
import Http
import Json.Encode exposing (bool, float, int, list, object, string, value)

type alias User =
  { username : String
  , password : String
  }

type Msg
  = Response (Result Http.Error String)

authenticate : User -> Cmd Msg
authenticate user =
  let
    body =
      value
        [ ("username", string user.username)
        , ("password", string user.password)
        ]
  in
    Http.send Response
      { method = "GET"
      , url = "https://example.com"
      , body = body
      , expect = Http.expectString -- Utiliser `expectJson` pour un format JSON
      , headers =
          [ Http.header "Authorization"
              ("Basic " ++ Http.base64Encode (user.username ++ ":" ++ user.password))
          ]
      }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Response (Ok response) ->
      ( { model | isAuthenticated = True }, Cmd.none )
    Response (Err error) ->
      ( model, Cmd.none )

view : Model -> Html Msg
view model =
  button [ onClick (authenticate { username = "john", password = "doe" }) ] [ text "Connexion" ]
```

Lorsque vous cliquez sur le bouton "Connexion", la fonction `authenticate` est appelée avec les identifiants de l'utilisateur. Le code génère une requête HTTP avec une méthode GET et un corps de requête qui inclut les identifiants encodés en base64. La réponse est gérée dans la fonction `update` et le modèle est mis à jour en fonction du résultat de la requête.

# Faire une plongée plus profonde:
L'authentification de base est l'une des méthodes d'authentification les plus anciennes pour les applications Web. Elle est utilisée pour valider l'identité de l'utilisateur en transmettant ses identifiants en clair à chaque requête. Cependant, cette méthode est considérée comme peu sécurisée car les identifiants peuvent être facilement interceptés.

Il existe d'autres méthodes d'authentification plus sécurisées telles que l'authentification par jeton qui utilise des clés de sécurité uniques pour chaque utilisateur. Cependant, l'authentification de base est encore utilisée pour des raisons de compatibilité avec les anciennes applications Web.

# Voir aussi:
- Documentation officielle Elm HTTP: https://package.elm-lang.org/packages/elm/http/latest/
- Tutoriel sur l'authentification de base avec Elm: https://abdulapopoola.com/2017/08/23/on-elm-the-state-of-basic-authentication/