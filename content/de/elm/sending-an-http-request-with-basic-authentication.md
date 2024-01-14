---
title:                "Elm: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Es gibt mehrere Gründe, warum man sich für das Versenden von HTTP-Anfragen mit Basic-Authentifizierung entscheiden könnte. Einer davon ist, dass dies eine einfache und effektive Methode ist, um eine zusätzliche Sicherheitsebene für eine Anwendung zu schaffen, indem man den Zugriff auf bestimmte Ressourcen beschränkt. Außerdem ermöglicht es die Verwendung von individuellen Anmeldeinformationen für jeden Benutzer.

# Wie geht's

Um eine HTTP-Anfrage mit Basic-Authentifizierung in Elm zu senden, müssen wir zuerst das Paket `elm/http` installieren. Dann können wir die Funktion `Http.send` verwenden, um eine Anfrage zu erstellen und zu senden. Der Authentifizierungsheader kann mithilfe der Funktion `Http.withHeader` hinzugefügt werden und die Anmeldeinformationen müssen als base64-kodierte Zeichenkette im Format `" Benutzername:Passwort "` angegeben werden.

```Elm
import Http
import Json.Decode exposing (..)
import Json.Encode

type Msg
    = RequestCompleted (Result Http.Error String)

sendRequest : Cmd Msg
sendRequest =
    Http.send RequestCompleted
        <| Http.get
            { url = "http://example.com/api/resource"
            , expect = Http.expectString decodeResponse
            , headers = [ Http.withHeader "Authorization" "Basic YWxhZGRpbjpvcGVuc2VzYW1l" ]
            }

decodeResponse : Decoder String
decodeResponse =
    decodeString
```

Die `sendRequest`-Funktion gibt einen Befehl zurück, der gesendet werden kann, um die Anfrage auszuführen. Wenn die Anfrage erfolgreich ist, gibt es ein `Ok`-Ergebnis mit der Antwort vom Server zurück, andernfalls ein `Err`-Ergebnis mit einer Fehlermeldung.

## Tief tauchen

Beim Senden einer HTTP-Anfrage mit Basic-Authentifizierung ist es wichtig zu beachten, dass diese Methode nicht als sicher gilt. Die Anmeldeinformationen werden als base64-kodierte Zeichenkette übermittelt, die relativ leicht entschlüsselt werden kann. Daher ist es ratsam, zusätzliche Sicherheitsmaßnahmen zu ergreifen, wie z.B. eine SSL-Verbindung herzustellen, um die Übermittlung der Anmeldeinformationen zu schützen.

Eine weitere wichtige Überlegung ist, dass die Anmeldeinformationen im Klartext im Code enthalten sind und somit potenziell von Unbefugten eingesehen werden können. Daher ist es empfehlenswert, die Anmeldeinformationen in einer separaten Konfigurationsdatei zu speichern und diese zur Laufzeit einzubinden.

# Siehe auch

- [Elm HTTP-Paket Dokumentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Base64 Kodierung](https://de.wikipedia.org/wiki/Base64)
- [HTTPS in Elm](https://supermario.github.io/elm-basics-the-missing-guide/09-https/)