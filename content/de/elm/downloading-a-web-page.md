---
title:                "Elm: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Warum

Das Herunterladen von Webseiten ist ein wichtiger Schritt beim Erstellen von Webanwendungen. Es ermöglicht uns, Daten von anderen Servern abzurufen und in unsere eigene Anwendung einzubinden. In diesem Blog-Beitrag werden wir lernen, wie wir mit Elm Webseiten herunterladen können.

# Wie es geht

Um eine Webseite mit Elm herunterzuladen, müssen wir zuerst das `Http` Modul importieren. Dann können wir die Funktion `Http.send` verwenden, um eine HTTP-Anfrage an die gewünschte URL zu senden.

```Elm
import Http
```

Dann können wir diese Funktion wie folgt aufrufen:

```Elm
Http.send request
```

Wir müssen jedoch zuerst ein `Http.Request` Objekt erstellen, das alle relevanten Informationen für unsere Anfrage enthält. Dieses Objekt hat verschiedene Felder wie `url`, `method`, `headers` und `body`. Je nach Art der Anfrage müssen wir diese Felder entsprechend setzen.

Hier ist ein Beispiel für eine GET-Anfrage:

```Elm
request : Http.Request
request =
    Http.request
        { method = "GET"
        , url = "https://www.example.com"
        , headers = []
        , body = Http.emptyBody
        }
```

Nachdem wir unsere Anfrage gesendet haben, erhalten wir eine `Cmd` zurück, die wir in unserem `update`-Funktion verarbeiten müssen. Wir können dann die empfangenen Daten verarbeiten und anzeigen, wie wir es in jedem anderen Fall auch tun würden.

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --...

        LoadingComplete response ->
            case response of
                HttpNetworkFailed err ->
                    ( model, Cmd.none ) -- handle error
                HttpBadUrl url ->
                    ( model, Cmd.none ) -- handle error
                HttpBadStatus code ->
                    ( model, Cmd.none ) -- handle error
                HttpBadBody err ->
                    ( model, Cmd.none ) -- handle error
                HttpGood response ->
                    ( { model | data = response.body }, Cmd.none ) -- handle successful response

        --...
```

# Tiefergehende Informationen

Der `Http`-Funktionsumfang von Elm ist ziemlich umfangreich und es gibt verschiedene Funktionen und Hilfsfunktionen, die wir nutzen können, um unsere HTTP-Anfragen zu verfeinern. Es gibt auch Möglichkeiten, wie wir mit Fehlerfällen umgehen können, die bei der Verarbeitung der Daten auftreten können.

Erfahrene Elm-Entwickler können auch die Typsicherheit von Elm nutzen, um sicherzustellen, dass unsere HTTP-Anfragen korrekt aufgebaut sind und die erwarteten Ergebnisse zurückgeben.

# Siehe auch

- [Elm Dokumentation zu Http](https://package.elm-lang.org/packages/elm/http/latest/)
- [Tutorial: How to Fetch Data from an API with Elm](https://medium.com@appsynth/tutorial-how-to-fetch-data-from-an-api-with-elm-37c28ba4d731)
- [HTTP Requests in Elm](https://guide.elm-lang.org/effects/http.html)