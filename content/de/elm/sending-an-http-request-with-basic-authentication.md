---
title:                "Senden einer HTTP-Anfrage mit Grundauthentifizierung"
html_title:           "Elm: Senden einer HTTP-Anfrage mit Grundauthentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit Grundauthentifizierung"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist ein häufiges Szenario bei der Entwicklung von Webanwendungen. Mit Hilfe von Elm können wir diese Aufgabe auf einfache und effiziente Weise erledigen, ohne uns um komplexe Authentifizierungslogik kümmern zu müssen.

## Wie geht's?

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Elm zu senden, müssen wir zunächst die Bibliothek "elm/http" importieren. Anschließend können wir die Funktion "sendWithAuth" verwenden, um die Anfrage zu konfigurieren und auszuführen.

**Hinweis:** Stellen Sie sicher, dass Sie die erforderlichen Berechtigungen haben, um auf die entsprechende Ressource zuzugreifen.

```elm
import Http
import String

-- Beispiel-URL für eine geschützte Ressource
url = "https://www.example.com/api/user"

-- Benutzername und Passwort für die Authentifizierung
username = "username"
password = "password"

-- Codieren Sie den Benutzernamen und das Passwort in Base64
encodedCredentials =
    String.join ":" [username, password]
        |> String.toBase64

-- Definieren Sie die Anfrage mit grundlegender Authentifizierung
request : Http.Request String
request =
    let
        config =
            Http.withBasicAuth encodedCredentials
                |> Http.expectString
    in
        Http.request { method = "GET", headers = [], url = url, body = Http.emptyBody, expect = config}

-- Senden der Anfrage und Verarbeiten der Antwort
Http.sendWithAuth (\result -> case result of
                                Ok response ->
                                    case response of 
                                        Http.BadUrl _ ->
                                            -- Fehlerbehandlung
                                        _ ->
                                            -- Verarbeiten Sie die Antwort hier
                                Err _ ->
                                    -- Fehlerbehandlung
                                ) request
```

Die obigen Code-Beispiele zeigen die Verwendung von "elm/http" und "elm/string" Bibliotheken, um eine Anfrage mit grundlegender Authentifizierung zu senden. Wir codieren den Benutzernamen und das Passwort in Base64, um sie als Teil des Authorization-Headers in der Anfrage zu senden.

## Tiefer Einblick

Die Funktion "withBasicAuth" aus der Bibliothek "elm/http" bietet uns eine einfache Möglichkeit, grundlegende Authentifizierung für unsere HTTP-Anfragen zu implementieren. Es nimmt die codierten Anmeldeinformationen als Argument und gibt eine Konfiguration für die Anfrage zurück, die den Authorization-Header enthält.

Bei der Verwendung von grundlegender Authentifizierung ist es wichtig, dass die Anmeldeinformationen sicher übertragen werden, da sie sonst leicht entschlüsselt werden können. Auch aus diesem Grund ist es wichtig, eine sichere Verbindung (HTTPS) zu verwenden, um sensible Daten zu übertragen.

## Siehe auch

- "Einführung in Elm" von Jan König (https://jan-konig.de/elm)
- Offizielle Elm-Dokumentation (https://guide.elm-lang.org/)
- Beispielprojekt mit grundlegender Authentifizierung in Elm (https://github.com/example-project)