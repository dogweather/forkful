---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Gleam: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man eine HTTP-Anfrage mit Grundauthentifizierung senden? In der heutigen Welt der Webentwicklung ist es unerlässlich, dass Anwendungen auf sichere Weise miteinander kommunizieren können. Die Verwendung von Grundauthentifizierung ermöglicht den Schutz sensibler Daten bei der Übertragung über das Internet.

## Wie man es macht

Um eine HTTP-Anfrage mit Grundauthentifizierung in Gleam zu senden, müssen Sie zunächst ein HTTP-Client-Modul importieren. Verwenden Sie dann die Funktion `BasicAuth` und übergeben Sie Ihre Benutzername und Passwort als Argumente.

```gleam
import http

// Ein Beispiel für eine HTTP-Anfrage mit Grundauthentifizierung
let request = http
  .get("https://meineanwendung.com/api/user", BasicAuth("benutzer123", "meinpasswort"))
  .send()
```

Das `BasicAuth`-Argument wird automatisch in das Anfrage-Header eingefügt, um die Authentifizierung zu ermöglichen. Sie können auch die `getAuthHeader`-Funktion verwenden, um das erstellte Header zu überprüfen oder zu ändern.

```gleam
let authHeader = BasicAuth(benutzername, passwort) |> http.getAuthHeader()
```

Die oben genannten Funktionen sind nur Beispiele, es gibt jedoch verschiedene andere Methoden, um HTTP-Anfragen mit Grundauthentifizierung in Gleam zu senden. Bitte schau dir die offizielle Dokumentation an, um weitere Details zu erfahren.

## Tiefer eintauchen

Bei der Verwendung von HTTP-Anfragen mit Grundauthentifizierung gibt es einige wichtige Dinge zu beachten. Zum Beispiel ist es wichtig, sicherzustellen, dass Ihre Anwendung über eine SSL-Verbindung verfügt, um die Sicherheit Ihrer Daten zu gewährleisten. Darüber hinaus sollten Sie bei der Verwendung von Grundauthentifizierung sicherstellen, dass Benutzername und Passwort sicher gespeichert und übertragen werden, um die Risiken von unbefugtem Zugriff zu minimieren.

## Siehe auch

- [Official Gleam Documentation](https://gleam.run/core/http.html#basic-auth)
- [HTTP Basic Authentication Explained](https://www.techopedia.com/definition/23974/basic-authentication)