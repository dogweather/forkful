---
title:                "Das Senden einer http-Anfrage"
html_title:           "Elm: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum

Wenn du ein Webentwickler bist, der eine benutzerfreundliche Programmiersprache mit einer starken Typisierung und einer sanften Lernkurve sucht, dann ist Elm die perfekte Wahl für dich. Mit Elm können wir auch HTTP-Anfragen senden, um Daten von anderen Webressourcen abzurufen. In diesem Artikel zeige ich dir, wie du das machen kannst.

## So geht's

Um eine HTTP-Anfrage in Elm zu senden, müssen wir zunächst das `Http`-Modul importieren. Dann können wir die Funktion `send` verwenden, um eine Anfrage an eine bestimmte URL zu senden. Die Antwort wird als ein `Result`-Typ zurückgegeben, der entweder eine erfolgreiche Antwort oder einen Fehler enthalten kann. Schauen wir uns das Ganze mal in einem Beispiel an:

```Elm
import Http

Http.send {
    method = "GET",
    headers = [],
    url = "https://myapi.com/users",
    body = Http.emptyBody
}
    |> Task.attempt handleResponse
```

Hier senden wir eine GET-Anfrage an die URL `https://myapi.com/users` und geben die Antwort an die Funktion `handleResponse` weiter. Wir können auch Parameter wie Headers, Body und mehr angeben, um die Anfrage anzupassen.

```Elm
Http.send {
    method = "POST",
    headers = [("Content-Type", "application/json")],
    url = "https://myapi.com/users",
    body = Http.jsonBody <| Encode.object <| {
        name = "John Doe",
        age = 28,
        email = "johndoe@example.com"
    }
}
    |> Task.attempt handleResponse
```

Hier senden wir eine POST-Anfrage an dieselbe URL, aber diesmal geben wir an, dass der Body in JSON-Format übergeben wird.

## Tiefere Einblicke

Bei der Verwendung von `Http.send` müssen wir auch sicherstellen, dass wir die notwendigen Berechtigungen für die Verwendung von HTTP-Anfragen haben. Dies kann entweder durch den `elm.json`-Datei-Eintrag `"dependencies"` oder durch den `elm-full.json`-Datei-Eintrag `"full-dependencies"` erreicht werden. Ohne diese Einträge wird dein Code nicht kompilieren und eine entsprechende Fehlermeldung ausgegeben werden.

Es ist auch wichtig zu beachten, dass `Http.send` asynchron ist, was bedeutet, dass es eine Aufgabe (Task) zurückgibt. Wir müssen also eine Funktion wie `Task.attempt` verwenden, um die Antwort zu erhalten.

## Siehe auch

- [Elm-Dokumentation zum `Http`-Modul](https://package.elm-lang.org/packages/elm/http/latest/)
- [Video-Tutorial zum Senden von HTTP-Anfragen in Elm](https://www.youtube.com/watch?v=7s8ex-4FyP8)
- [Blogbeitrag über HTTP-Anfragen in Elm](https://www.simonherteby.com/blog/2017/05/02/elm-sending-http-request.html)