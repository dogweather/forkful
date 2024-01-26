---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:43.739089-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung senden deinen Nutzernamen und das Passwort kodiert im HTTP-Header, um sich bei einem Webdienst anzumelden. Programmierer nutzen das für einfache Zugriffssicherung, wenn Ressourcen geschützt, aber nicht top-secret sind.

## How to:
Der folgende Gleam-Code zeigt, wie man eine HTTP-Anfrage mit Basisauthentifizierung sendet:

```gleam
import gleam/http.{Request, basic_auth_header}
import gleam/httpc.{send}

pub fn send_auth_request(username: String, password: String) -> Result {
  let auth_header = basic_auth_header(username, password)
  let request = Request(
    method: "GET",
    headers: [auth_header],
    url: "https://meinapi.de/geheime_daten",
  )

  send(request)
}

pub fn example() {
  case send_auth_request("meinUsername", "meinPassword") {
    Ok(response) -> {
      io.println("Erfolg! Daten erhalten: ")
      io.println(response.body)
    }
    Error(error) -> {
      io.println("Fehler bei der Anfrage: ")
      io.println(error)
    }
  }
}
```
Läuft das Beispiel, erhältst du etwa diese Ausgabe:

```
Erfolg! Daten erhalten:
{"geheime": "Daten", "anzahl": 42}
```

oder bei einem Fehler:

```
Fehler bei der Anfrage:
{reason: "Unauthorized", ...}
```

## Deep Dive
Die Basisauthentifizierung wurde Teil des HTTP/1.0-Standards und ist seitdem ein einfacher Weg, Zugriff zu kontrollieren. Es gilt aber als unsicher, weil das Passwort einfach zu entschlüsseln ist, sollte jemand den HTTP-Verkehr abfangen. Deswegen: Nutze HTTPS und setze, wo möglich, auf sicherere Authentifizierungsmethoden wie OAuth.

Gleam, eine statisch typisierte Sprache, erlaubt besseres Fehlermanagement durch sein `Result`-Typ-System, wodurch man sicherer programmieren kann. Die Basisauthentifizierung in Gleam wird einfach mittels `basic_auth_header`-Funktion umgesetzt.

Alternativen zur Basisauthentifizierung umfassen Token-basierte Authentifizierung, OAuth und andere, die Flexibilität oder Sicherheit je nach Anforderung optimieren.

## Siehe auch:

- Gleam HTTP library Dokumentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Richtlinien zur Basisauthentifizierung und ihre Risiken: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
