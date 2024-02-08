---
title:                "Einen HTTP-Request senden"
aliases:
- de/elm/sending-an-http-request.md
date:                  2024-01-20T17:59:39.752330-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen werden genutzt, um mit Webservern zu kommunizieren – Daten zu holen und zu senden. Programmierer brauchen diese, um Webanwendungen zu erstellen, die mit Backend-Diensten interagieren.

## How to:
Elm macht HTTP-Anfragen übersichtlich. Hier ist ein einfaches Beispiel, wie man eine GET-Anfrage schickt:

```Elm
module Main exposing (..)

import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

getUser : Cmd Msg
getUser =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/users/1"
        , expect = Http.expectJson GotUser userDecoder
        }

type Msg
    = GotUser (Result Http.Error User)

-- Add relevant update and view functions here.
```

Ergebnis (abhängig von der Server-Antwort):

```Elm
GotUser (Ok { id = 1, name = "Leanne Graham" })
```

## Deep Dive
Elm bietet eine starke, typensichere Art, HTTP-Anfragen zu machen. Ursprünglich bot Elm nur Low-Level-Funktionen, aber mit der Zeit und mehreren Releases, einschließlich des aktuellen Elm 0.19.1, entwickelte sich die HTTP-Library zu einem robusten Werkzeug mit hilfreichen Abstraktionen.

Alternativen zu Elm's eingebauter `Http` Bibliothek sind selten, da Elm eine begrenzte Interoperabilität mit JavaScript hat, um die Zuverlässigkeit zu maximieren. In der Praxis bedeutet das, dass Elm-Entwickler meist auf das offizielle `elm/http` Paket angewiesen sind.

Das Senden einer HTTP-Anfrage erfolgt über eine `Cmd`, die eine asynchrone Aktion repräsentiert. Elm handhabt diese Aktionen im `update`-Teil der Anwendung, was den Side-Effects Management simplifiziert und zu einem vorhersehbaren Ablauf führt.

## See Also
- Elm HTTP package documentation: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm Lang official guide: [guide.elm-lang.org](https://guide.elm-lang.org)
