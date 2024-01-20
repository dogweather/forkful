---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basic Authentication ist ein Prozess, bei dem sich ein Client beim Server authentifiziert, indem er ein Benutzername-Passwort-Paar im Klartext übermittelt. Dieses Verfahren wird häufig verwendet, wenn ein zuverlässiger Übertragungskanal (z.B. HTTPS) vorhanden ist, um die Sicherheit der Daten zu gewährleisten.

## Wie geht's:

Mit Go’s Standardbibliothek ist es einfach, eine HTTP-Anfrage mit Basic Authentication zu senden. Hier zeigt ein kurzes Codebeispiel, wie es funktioniert:

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	req, err := http.NewRequest("GET", "http://example.com", nil)
	if err != nil {
		log.Fatal(err)
	}
	req.SetBasicAuth("username", "password")
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()
	fmt.Println("Response status:", resp.Status)
}
```
Die Ausgabe dieses Programm wäre:

```Go
Response status: 200 OK
```

## Tief tauchen:

Historisch gesehen hat sich Basic Authentication als einfache Methode zur Authentifizierung von Clients gegenüber Servern etabliert. Trotz seiner Schwächen, insbesondere der Übermittlung von Kennwortinformationen im Klartext, bleibt es im Web weit verbreitet.

Alternativen zu Basic Authentication gibt es viele, darunter OAuth, Cookie-Based Authentication und Token-Based Authentication. Jede dieser Methoden hat ihre eigenen Vor- und Nachteile.

Wenn Sie in Go eine HTTP-Anfrage mit Basic Authentication senden, sollten Sie beachten, dass die Anmeldeinformationen im Klartext gesendet werden. Daher sollten Sie immer eine sichere Verbindung (z.B. HTTPS) verwenden, um diese Anfragen zu senden.

## Siehe auch:

Für weiterführende Informationen können diese Links nützlich sein:

- [Go Standardbibliothek - Netzwerk](https://golang.org/pkg/net/)
- [HTTP-Authentifizierung](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)

Obwohl Basic-Authentication seine Schwachstellen hat, kann es in sicheren Verbindungen durchaus nützlich und praktikabel sein. Bei unsicherer Verbindung sollten aber stets sicherere Alternativen in Erwägung gezogen werden. Die Go-Standardbibliothek bietet dennoch eine einfache und effektive Möglichkeit, HTTP-Anfragen mit Basic-Authentication zu senden.