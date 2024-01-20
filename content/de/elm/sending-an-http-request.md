---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP-Anfragen in Elm

## Was & Warum?

Beim Senden einer HTTP-Anfrage stellt ein Client eine Verbindung zu einem Webserver her und bittet um bestimmte Daten. Als Programmierer tun wir dies, um Inhalte und Daten von Servern oder APIs zu extrahieren.

## Wie zu:

In Elm können wir das `Http`-Paket verwenden, um HTTP-Anfragen zu senden. Hier ist ein einfacher Code-Ausschnitt, der eine GET-Anfrage an eine URL sendet:

```Elm
import Json.Decode
import Http

fetchData : String -> Cmd Msg
fetchData url =
    Http.get
        { url = url
        , expect = Http.expectJson GotResponse Json.Decode.string
        }

type Msg
    = GotResponse (Result Http.Error String)
```

In diesem Fall erwarten wir zurückgegebene Daten als Json.

## Vertiefter Einblick 

Historisch gesehen wurden HTTP-Anfragen in Elm über das `Http`-Modul der Elm-Architektur implementiert. Da Elm eine reine funktionale Sprache ist, gehen wir trotz Verwendung von Befehlen immer noch rein funktional vor.

An Alternativen zu `Http.get` gibt es `Http.post`, `Http.put`, `Http.delete` usw. für unterschiedliche Interaktionen mit dem Server.

Was die Implementierungsdetails betrifft, so haben wir in unserem obigen Code den Befehl `Http.get` mit einem URL und einer erwarteten Antwort. Die Antwort wird dann vom Decoder analysiert, der die Json-Daten entsprechend verarbeiten kann.

## Siehe auch:

Für weitere Details:
- [Elm Guide](https://guide.elm-lang.org/): Offizielles Lehrbuch für Elm.
- [Elm Http Package Dokumentation](https://package.elm-lang.org/packages/elm/http/latest/): Dokumentation zum `Http`-Paket in Elm.
- [Elm Json Decode Explained](https://korban.net/posts/elm/2018-12-28-elm-json-decode-explained/): Erklärung zu Json Decoding in Elm.