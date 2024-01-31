---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:46.764176-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP Anfragen mit Basic Authentication senden Daten über einen geschützten Kanal, indem sie Benutzername und Passwort mit der Anfrage mitgeben. Programmierer nutzen dies, um über APIs sicheren Zugriff auf Webdienste zu gewährleisten.

## Wie geht das:
```Go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	// Deine Zugangsdaten festlegen
	username := "DeinBenutzername"
	password := "DeinPasswort"

	// Base64 Kodierung der Zugangsdaten
	basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte(username + ":" + password))

	// HTTP Request erstellen
	req, err := http.NewRequest("GET", "https://deine-api.de/daten", nil)
	if err != nil {
		fmt.Println("Fehler beim Erstellen der Anfrage:", err)
		return
	}

	// Basic Authentication Header setzen
	req.Header.Add("Authorization", basicAuth)

	// HTTP Client erzeugen und Anfrage senden
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println("Fehler beim Senden der Anfrage:", err)
		return
	}
	defer resp.Body.Close()

	// Status der Antwort ausgeben
	fmt.Println("Antwort Status:", resp.Status)
}
```
Ausgabe könnte sein:
```
Antwort Status: 200 OK
```

## Deep Dive:
Sending HTTP requests with basic authentication is a straightforward process that stems from the early days of web authentication. The 'Authorization' header transports credentials encoded in Base64 - simple and not secure by modern standards. It's suitable for internal networks or with HTTPS, which encrypts the entire request.

Alternatives like OAuth 2.0 and JWT (JSON Web Tokens) provide more secure and flexible options for authorization and are widely used in modern applications. Basic authentication, however, remains prevalent due to its simplicity and ease of implementation - just a header added to your HTTP request.

When implementing, it is vital to consider encryption and the sensitivity of the data you're transmitting. Basic authentication without encryption (like HTTP without SSL/TLS) should be avoided due to the risk of credential interception.

## Siehe Auch:
- Go's official HTTP package documentation: [net/http](https://pkg.go.dev/net/http)
- Basic authentication scheme as per RFC7617: [RFC 7617](https://tools.ietf.org/html/rfc7617)
- More secure alternatives: [OAuth 2.0](https://oauth.net/2/), [JWT](https://jwt.io/)
- Understanding Base64 Encoding: [Base64 Encoding Explained](https://en.wikipedia.org/wiki/Base64)
