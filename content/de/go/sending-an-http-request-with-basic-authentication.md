---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Go: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was & Warum?

Das Senden eines HTTP-Anforderung mit grundlegender Authentifizierung ist ein wichtiger Schritt beim Programmieren von Webanwendungen. Es ermöglicht den sicheren Austausch von Daten zwischen dem Client und dem Server. Programmierer nutzen grundlegende Authentifizierung, um sicherzustellen, dass die Daten, die sie senden, authentisch sind und nur von berechtigten Benutzern abgerufen werden können.

Wie geht das?

Go macht es einfach, eine HTTP-Anforderung mit grundlegender Authentifizierung zu senden. Verwende einfach das Paket "net/http" und die "Request.BasicAuth" Methode, um die Anfrage mit Anmeldedaten zu versehen. Im folgenden Beispiel senden wir eine GET-Anforderung an die URL "http://example.com" mit dem Benutzernamen "user" und dem Passwort "password":

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	url := "http://example.com"
	req, err := http.NewRequest("GET", url, nil)
	req.SetBasicAuth("user", "password")
	client := http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	fmt.Println(resp.StatusCode)
}
```

Dies wird eine 200-Statuscode als Antwort zurückgeben, was bedeutet, dass die Anfrage erfolgreich war.

Tiefer tauchen

Grundlegende Authentifizierung ist ein Protokoll, das von HTTP verwendet wird, um Benutzernamen und Passwörter zu überprüfen. Es wird oft verwendet, um den Zugriff auf vertrauliche oder geschützte Informationen zu beschränken. Eine alternative Methode zur Authentifizierung ist die Verwendung von Tokens, bei denen ein einmaliger Code anstelle von Benutzername und Passwort verwendet wird. Beim Senden von HTTP-Anforderungen mit grundlegender Authentifizierung ist es wichtig, dass die HTTP-Verbindung über HTTPS gesichert ist, um zu verhindern, dass die Anmeldeinformationen von Dritten abgefangen werden.

Siehe auch

- [Das net/http Paket in der Go-Dokumentation](https://golang.org/pkg/net/http/)
- [Ein Tutorial zur grundlegenden Authentifizierung in Go](https://www.sohamkamani.com/golang/basic-authentication/)
- [Eine Einführung in Go von der offiziellen Website](https://golang.org/doc/tutorial/getting-started)