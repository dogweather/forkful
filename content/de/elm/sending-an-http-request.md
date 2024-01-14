---
title:                "Elm: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich überhaupt mit dem Senden von HTTP-Anfragen beschäftigen? Nun, in der heutigen Zeit kommunizieren viele Anwendungen und Dienste über das Internet miteinander. Das Senden von HTTP-Anfragen ermöglicht es uns, Daten von anderen Diensten abzurufen oder an diese zu senden. Dies kann sehr hilfreich sein, um beispielsweise Daten von einer API abzurufen oder Formulardaten an einen Server zu senden.

# So geht's

Um eine HTTP-Anfrage in Elm zu senden, benötigen wir das [Http](https://package.elm-lang.org/packages/elm/http/latest/) Modul. Dieses bietet uns Funktionen an, um Anfragen an eine URL zu senden und die Antwort zu verarbeiten.

```Elm
import Http
import Json.Decode exposing (..)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    decode User
        |> field "name" string
        |> field "age" int

getUser : String -> Cmd Msg
getUser id =
    Http.get
        { url = "https://example.com/users/" ++ id
        , expect = Http.expectJson userDecoder
        }
```

In diesem Beispiel importieren wir das `Http` Modul und das `Json.Decode` Modul, um die Antwort der Anfrage zu dekodieren. Wir definieren auch einen `User` Datentyp und ein `userDecoder` Decoder für das Dekodieren der Antwort. Die Funktion `getUser` sendet dann eine GET-Anfrage an die angegebene URL und erwartet eine JSON-Antwort, die mit dem Decoder dekodiert wird.

# Tiefere Einblicke

Das `Http` Modul bietet noch weitere nützliche Funktionen wie `Http.post`, `Http.put` und `Http.delete`, um POST-, PUT- und DELETE-Anfragen zu senden. Außerdem ermöglicht es uns, Anfrage-Header und Anfrage-Body anzugeben und festzulegen, wie die Antwort verarbeitet werden soll. Das [Elm Guide](https://guide.elm-lang.org/) bietet eine detaillierte Erklärung und Beispiele für die Verwendung des `Http` Moduls.

# Siehe auch

- [Elm Guide: HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm Package Docs: Http](https://package.elm-lang.org/packages/elm/http/latest/)