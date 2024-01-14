---
title:                "Go: Eine http-Anfrage mit Basis-Authentifizierung senden"
simple_title:         "Eine http-Anfrage mit Basis-Authentifizierung senden"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden eines HTTP-Requests mit Basic Authentication ist eine fundamentale Fähigkeit, die jeder Go-Programmierer kennen sollte. Durch die Verwendung von Basic Authentication können Benutzer ihre Identität überprüfen und auf geschützte Ressourcen zugreifen, was für viele Anwendungen unerlässlich ist.

## Wie

Um eine HTTP-Anfrage mit Basic Authentication zu senden, können Sie die Standardbibliothek von Go verwenden. Zunächst müssen Sie eine Anfrage erstellen und die URL der Ressource angeben, auf die Sie zugreifen möchten. Anschließend müssen Sie die erforderlichen Anmeldeinformationen mithilfe der Methode `SetBasicAuth` angeben. Nach dem Senden der Anfrage erhalten Sie eine Antwort mit dem richtigen Statuscode und gegebenenfalls den angeforderten Daten.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	// URL der Ressource
	url := "https://example.com/api/resource"

	// Neue Anfrage erstellen
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		panic(err)
	}

	// Anmeldeinformationen angeben
	req.SetBasicAuth("Benutzername", "Passwort")

	// Request senden
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	// Antwort lesen
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	// Ausgabe der empfangenen Daten
	fmt.Println(string(body))
}
```

Dieses Beispiel zeigt, wie einfach es ist, eine HTTP-Anfrage mit Basic Authentication in Go zu senden. Sie können auch die HTTP-Client-Bibliothek "net/http" verwenden, um die Anfrage und die Anmeldeinformationen zu verwalten. Weitere Informationen dazu finden Sie in der Dokumentation.

## Deep Dive

Wenn Sie tiefer in das Thema einsteigen wollen, gibt es einige wichtige Dinge zu beachten. Zum Beispiel muss die gesendete Anfrage eine gültige Authentifizierungsmethode angeben, sonst wird der Server die Anfrage ablehnen. Es ist auch möglich, ein "Authorization"-Header anstatt der `SetBasicAuth`-Methode zu verwenden, um die Anmeldeinformationen anzugeben.

Eine weitere wichtige Überlegung ist die Sicherheit. Basic Authentication sollte nur in Verbindung mit einer sicheren Verbindung (HTTPS) verwendet werden, da die Anmeldeinformationen sonst im Klartext übertragen werden. Außerdem ist zu beachten, dass Basic Authentication keine zusätzliche Verschlüsselung bietet und daher nicht für die Übertragung sensibler Daten geeignet ist.

## Siehe auch

Weitere Informationen zum Senden von HTTP-Anfragen mit Basic Authentication finden Sie in der offiziellen Dokumentation von Go:

- https://golang.org/pkg/net/http/
- https://golang.org/pkg/net/http/#Request.SetBasicAuth
- https://golang.org/pkg/net/http/#Client

Sie können auch die folgenden Links besuchen, um mehr über HTTP-Authentifizierung und Sicherheit zu erfahren:

- https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
- https://www.owasp.org/index.php/Authentication_Cheat_Sheet