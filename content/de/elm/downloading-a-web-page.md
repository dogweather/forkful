---
title:                "Herunterladen einer Webseite"
html_title:           "Elm: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand eine Webseite herunterladen wollen? Nun, es gibt verschiedene Gründe dafür. Vielleicht möchte man die Webseite offline lesen, ohne ständig eine Internetverbindung zu haben. Oder man möchte die Datei aufbewahren, um später darauf zugreifen zu können, auch wenn die Webseite offline geht oder gelöscht wird. In jedem Fall ist das Herunterladen einer Webseite ein nützliches Werkzeug für diejenigen, die gerne im Internet surfen.

## Wie es geht

Um eine Webseite mit Elm herunterzuladen, müssen wir das `elm/http` Paket installieren. Dieses Paket bietet Funktionen zum Ausführen von HTTP-Anfragen, einschließlich des Herunterladens von Webseiten. Zuerst importieren wir das Paket mit `import Http`, dann definieren wir eine Funktion `downloadPage`, die eine URL als String akzeptiert:

```Elm
import Http

downloadPage : String -> Cmd Msg
downloadPage url =
    Http.get
        { url = url
        , expect = Http.expectString GotPage
        }
```

Diese Funktion verwendet die `Http.get` Funktion aus dem `elm/http` Paket, um eine GET-Anfrage an die angegebene URL zu senden. Um die Antwort zu verarbeiten, verwenden wir die `expectString` Funktion, die eine `Decoder` Funktion erwartet. In diesem Fall haben wir die `GotPage` Nachricht definiert, um die heruntergeladene Seite zu empfangen.

Als nächstes müssen wir unsere `GotPage` Nachricht und die zugehörige `Decoder` Funktion definieren:

```Elm
type Msg
    = GotPage (Result Http.Error String)

decodePage : Decoder String
decodePage =
    Decode.string
```

Die `GotPage` Nachricht nimmt ein `Result` entgegen, das entweder eine Fehlermeldung oder die heruntergeladene Seite enthält. Die `decodePage` Funktion erwartet einen String als Ergebnis und gibt einfach den empfangenen String zurück.

Schließlich müssen wir unsere `downloadPage` Funktion in unserer `update` Funktion ausführen:

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPage result ->
            case result of
                Ok page ->
                    ( { model | page = page }, Cmd.none )
                Err error ->
                    ( model, Cmd.none )

        -- Aufruf der Funktion zum Herunterladen der Seite
        DownloadPage ->
            -- Hier muss die URL der gewünschten Seite angegeben werden
            ( model, downloadPage "https://example.com" )
```

Sobald wir unsere `DownloadPage` Nachricht auslösen, führt die `update` Funktion unsere `downloadPage` Funktion aus. Wenn die Seite erfolgreich heruntergeladen wird, wird die `GotPage` Nachricht ausgelöst, und wir aktualisieren unser Modell mit der heruntergeladenen Seite.

## Tiefer Einblick

Um tiefer in die Arbeit mit dem Herunterladen von Webseiten mit Elm einzusteigen, können wir uns das `elm/url` Paket genauer ansehen. Dieses Paket bietet Funktionen zum Parsen von URLs, was nützlich sein kann, um bestimmte Teile einer heruntergeladenen Seite zu extrahieren. Wir können auch mit `elm/time` arbeiten, um Zeitstempel zu generieren und das Herunterladen von Webseiten zu planen.

## Siehe auch

- Offizielle Dokumentation für das `elm/http` Paket: https://package.elm-lang.org/packages/elm/http/latest/
- Offizielle Dokumentation für das `elm/url` Paket: https://package.elm-lang.org/packages/elm/url/latest/
- Offizielle Dokumentation für das `elm/time` Paket: https://package.elm-lang.org/packages/elm/time/latest/