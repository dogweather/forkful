---
title:                "Senden einer http-Anfrage"
html_title:           "Elm: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was ist eine HTTP-Anfrage und warum machen Programmierer sie?

Eine HTTP-Anfrage ist eine Anfrage, die ein Computer an einen Server sendet, um Daten oder Ressourcen anzufordern. Programmierer nutzen HTTP-Anfragen, um mit externen APIs oder Webseiten zu interagieren, um Daten zu erhalten oder zu manipulieren.

## Wie geht das?

```Elm
import Http
import Json.Decode exposing (..)

url : String
url = "https://jsonplaceholder.typicode.com/posts"

-- eine GET-Anfrage mit Http.get
Http.get url
  |> Http.send GotPosts


type Msg
  = GotPosts (Result Http.Error (List Post))

type alias Post =
  { userId : Int
  , id : Int
  , title : String
  , body : String
  }

view : Model -> Html Msg
view model =
  case model.posts of
    Err _ ->
      text "Error getting posts"
    Ok posts ->
      div [] (List.map renderPost posts)

renderPost : Post -> Html Msg
renderPost post =
  div []
    [ text ("Author: " ++ toString post.userId)
    , text ("Title: " ++ post.title)
    , text ("Body: " ++ post.body)
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPosts response ->
      case response of
        Ok posts ->
          ( { model | posts = Ok posts }, Cmd.none )
        Err _ ->
          ( { model | posts = Err "Could not get posts" }, Cmd.none )

``` 

Der obige Code zeigt, wie mit Elm eine HTTP-Anfrage an die API von "jsonplaceholder.typicode.com" gesendet werden kann. Wir importieren das Http-Paket, um HTTP-Anfragen zu ermöglichen, und das Json.Decode-Paket, um die empfangenen Daten zu interpretieren. Dann definieren wir eine URL und senden eine GET-Anfrage mit Http.get. Die Antwort wird in der update-Funktion verarbeitet und gerendert.

## Tiefgehende Informationen

HTTP-Anfragen sind ein grundlegendes Konzept für die Webentwicklung und wurden erstmals in den 1990er Jahren eingeführt. Es gibt auch alternative Möglichkeiten für HTTP-Anfragen, wie zum Beispiel Ajax oder Fetch, aber Elm bietet eine einfache und sichere Möglichkeit, um mit externen APIs zu kommunizieren.

Ein wichtiger Punkt bei der Verwendung von HTTP-Anfragen ist die Verarbeitung der empfangenen Daten. Elm bietet hierfür das Json.Decode-Paket, um sicherzustellen, dass die erhaltenen Daten das erwartete Format haben.

## Siehe auch

- [Elm Dokumentation zu HTTP-Anfragen](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial zu HTTP-Anfragen mit Elm](https://guide.elm-lang.org/effects/http.html)
- [Alternativen zu HTTP-Anfragen in Elm](https://package.elm-lang.org/packages/elm-lang/virtual-dom/latest/VirtualDom#task)