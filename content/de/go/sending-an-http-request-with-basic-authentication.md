---
title:                "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
html_title:           "Go: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

## Warum sich für Grundauthentifizierung bei HTTP-Anfragen entscheiden?
 
Die Grundauthentifizierung ist eine gängige Methode, um die Sicherheit von HTTP-Anfragen zu gewährleisten. Insbesondere wird sie oft bei der Übertragung von sensiblen Nutzerdaten wie Benutzernamen und Passwörtern verwendet. Sie stellt eine einfache Möglichkeit dar, um die Daten vor unautorisiertem Zugriff zu schützen.

# How To

## Schritt 1: Einrichtung der HTTP-Anfrage

Zunächst müssen wir eine standardmäßige HTTP-Anfrage in Go erstellen. Dazu verwenden wir die "net/http" Bibliothek:

```Go
req, err := http.NewRequest("GET", url, nil)
```

In diesem Fall erstellen wir eine GET-Anfrage an die URL, die wir in der Variable "url" gespeichert haben. Die Option "nil" gibt an, dass wir keine Daten mit der Anfrage übermitteln möchten.

## Schritt 2: Hinzufügen der Grundauthentifizierung

Um die Grundauthentifizierung zu verwenden, müssen wir dem Anfrageobjekt einen Basis Authentifizierungs-Header hinzufügen. Dazu rufen wir die Methode "SetBasicAuth" auf und übergeben Benutzername und Passwort als Parameter:

```Go
req.SetBasicAuth(username, password)
```

In diesem Beispiel gehen wir davon aus, dass der Benutzername und das Passwort in den Variablen "username" und "password" gespeichert sind. Wenn die Anfrage ausgeführt wird, wird der Authentifizierungs-Header automatisch zum Request hinzugefügt.

## Schritt 3: Ausführen der Anfrage und Verarbeiten der Antwort

Nachdem wir unsere Anfrage konfiguriert haben, können wir sie ausführen und die Antwort verarbeiten. Dazu können wir die "http" Bibliothek verwenden und die Methode "Do" aufrufen:

```Go
resp, err := http.DefaultClient.Do(req)
```

Die Antwort wird in der Variable "resp" gespeichert und kann nun verarbeitet werden. Zum Beispiel können wir auf den "StatusCode" zugreifen, um zu überprüfen, ob die Anfrage erfolgreich war:

```Go
if resp.StatusCode == 200 {
    // Anfrage war erfolgreich
}
```

# Deep Dive

## Weitere Details zur Grundauthentifizierung von HTTP-Anfragen

Die Grundauthentifizierung ist ein einfaches, aber anfälliges Verfahren. Da Benutzername und Passwort unverschlüsselt im Header der Anfrage übertragen werden, können sie leicht von Dritten abgefangen werden. Aus diesem Grund sollte die Grundauthentifizierung niemals als alleinige Sicherheitsmaßnahme verwendet werden.

In Go können wir die Grundauthentifizierung auch manuell zum Header hinzufügen, indem wir die Methode "Set" aufrufen und den entsprechenden Schlüssel- und Wertepaare übergeben:

```Go
req.Header.Set("Authorization", "Basic " + base64.StdEncoding.EncodeToString([]byte(username+":"+password)))
```

Um sicherzustellen, dass die Anfrage über eine sichere Verbindung gesendet wird, können wir auch die Methode "TLSClientConfig" verwenden, um die erforderlichen TLS-Einstellungen anzugeben.

# Siehe auch

- Offizielle Dokumentation zu HTTP-Anfragen mit Go: https://golang.org/pkg/net/http/
- Tutorial zur Verwendung von Basisauthentifizierung in Go: https://www.codementor.io/@slavko/basicauthenticationin-golang-o19ejv9zv
- Sicherheitsaspekte bei der Verwendung von Grundauthentifizierung: https://www.owasp.org/index.php/Testing_for_Basic_Authentication_(OWASP-AT-004)