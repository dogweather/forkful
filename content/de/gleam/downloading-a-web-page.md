---
title:                "Herunterladen einer Webseite"
html_title:           "Gleam: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich die Mühe machen, eine Webseite herunterzuladen? Nun, es gibt viele mögliche Gründe, einschließlich des Wunsches, die Seite offline zu lesen oder bestimmte Informationen aus der Seite zu extrahieren.

## Wie geht's

Um eine Webseite in Gleam herunterzuladen, verwenden wir die `HttpGet` Funktion. Zum Beispiel:

```Gleam
let result = Http.get("https://www.example.com")

result |> Log.info
```

Das Ergebnis der `HttpGet` Funktion ist ein `Http.Response`-Datentyp. Wir können darauf zugreifen, indem wir das `Body` Feld des `Http.Response`-Records verwenden:

```Gleam
// Beispiel: Zugriff auf den HTML-Inhalt der Webseite
result.body 
|> String.lines
|> Enum.take(10)
|> Log.info
```

Das Ergebnis der `HttpGet` kann auch fehlschlagen, daher ist es wichtig, entsprechende Fehlerbehandlung zu implementieren. Hier ist ein Beispiel, bei dem wir das `Http.expectOk` Helper-Funktion verwenden:

```Gleam
let result = Http.get("https://www.example.com") |> Http.expectOk

result.body |> Log.info
```

## Deep Dive

Die `HttpGet` Funktion implementiert einen HTTP-Client, der die Webseite herunterlädt und die entsprechenden HTTP-Response-Informationen zurückgibt. Dies ermöglicht es uns, jederzeit auf beliebige Webseiten zuzugreifen und ihre Inhalte zu verarbeiten. Um mehr über die `HttpGet`-Funktion und die Möglichkeiten der Manipulation von Webseiten zu erfahren, können Sie die offizielle Gleam-Dokumentation besuchen.

## Siehe auch

- [Gleam-Http-Verteilungsdokumentation](https://gleam.run/documentation/stdlib/http.html)
- [Einführung in die Gleam-Programmierung](https://gleam.run/getting-started/introduction.html)
- [Offizielle Gleam-Website](https://gleam.run/)