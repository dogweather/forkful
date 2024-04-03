---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:57.833563-07:00
description: "Wie geht das: Um eine HTTP-Anfrage mit Basisauthentifizierung in Go\
  \ zu stellen, m\xFCssen Sie Ihre Anfrage-Header so gestalten, dass sie das Feld\u2026"
lastmod: '2024-03-13T22:44:53.287285-06:00'
model: gpt-4-0125-preview
summary: "Um eine HTTP-Anfrage mit Basisauthentifizierung in Go zu stellen, m\xFC\
  ssen Sie Ihre Anfrage-Header so gestalten, dass sie das Feld `Authorization` enthalten,\
  \ gef\xFCllt mit Ihren Anmeldeinformationen im richtigen Format."
title: Eine HTTP-Anfrage mit Basisauthentifizierung senden
weight: 45
---

## Wie geht das:
Um eine HTTP-Anfrage mit Basisauthentifizierung in Go zu stellen, müssen Sie Ihre Anfrage-Header so gestalten, dass sie das Feld `Authorization` enthalten, gefüllt mit Ihren Anmeldeinformationen im richtigen Format. Unten finden Sie ein Beispiel, das zeigt, wie Sie eine GET-Anfrage an einen API-Endpunkt senden, der eine Basisauthentifizierung erfordert:

```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com/api/data", nil)
	if err != nil {
		panic(err)
	}

	username := "yourUsername"
	password := "yourPassword"
    // Anmeldeinformationen codieren
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
    // Authorization-Header festlegen
	req.Header.Add("Authorization", "Basic " + auth)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	fmt.Println("Antwortstatus:", resp.Status)
}
```

Wenn Sie diesen Code ausführen, wird eine GET-Anfrage an die angegebene URL mit dem erforderlichen Autorisierungs-Header gesendet. Die Ausgabe sieht je nach Ihrem Endpunkt und Dienst etwa so aus:

```
Antwortstatus: 200 OK
```

## Tiefergehende Betrachtung
Basisauthentifizierung bei HTTP-Anfragen ist eine weit unterstützte Methode zur Durchsetzung von Zugriffskontrollen auf Webressourcen. Sie sendet einfach einen Benutzernamen und ein Passwort mit jeder Anfrage, was sie einfach zu implementieren macht, aber nicht die sicherste verfügbare Methode ist. Ein großer Nachteil ist, dass die Anmeldeinformationen im Klartext gesendet werden (da Base64 leicht decodiert werden kann), es sei denn, sie wird in Verbindung mit SSL/TLS verwendet. Dies kann potenziell sensible Informationen Man-in-the-Middle-Angriffen aussetzen.

In Go involviert das Senden dieser Anfragen die direkte Manipulation des `Authorization`-Headers. Während Golangs Standardbibliothek (`net/http`) leistungsstarke Primitiven für den Umgang mit HTTP(s)-Kommunikation zur Verfügung stellt, ist sie vergleichsweise niedrigstufig, was bedeutet, dass Entwickler verschiedene Aspekte der HTTP-Anfrage-/Antwortbehandlung manuell handhaben müssen. Das gibt Programmierern viel Flexibilität, bedeutet aber auch, dass man die Sicherheitsimplikationen, Codierung und korrekte Headerverwaltung genauer beachten muss.

Für Anwendungen, die eine höhere Sicherheit erfordern, sollten fortgeschrittenere Authentifizierungssysteme wie OAuth2 oder JWT (JSON Web Tokens) in Betracht gezogen werden. Diese Ansätze bieten robustere Sicherheitsfunktionen und werden breit unterstützt in modernen APIs und Diensten. Golangs wachsendes Ökosystem beinhaltet zahlreiche Bibliotheken und Tools (wie `golang.org/x/oauth2` unter anderen), um diese sichereren Authentifizierungsmethoden zu erleichtern, was es für Entwickler einfacher macht, sichere, effektive und moderne Autorisierungsmechanismen in ihren Anwendungen zu implementieren.
