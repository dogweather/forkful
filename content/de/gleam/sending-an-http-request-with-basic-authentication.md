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

Was ist es & Warum?

Das Senden einer HTTP-Anforderung mit Basic-Authentifizierung ist ein wichtiger Teil der Entwicklung von Webanwendungen. Es ermöglicht es Programmen, sich bei einem Server zu authentifizieren und Daten sicher zu übermitteln. Programmierer verwenden dies, um sicherzustellen, dass nur autorisierte Benutzer oder Programme auf bestimmte Inhalte oder Funktionen zugreifen können.

Wie geht man vor?
Gleam hat eine integrierte HTTP-Bibliothek, die das Senden von HTTP-Anforderungen mit Basic-Authentifizierung sehr einfach macht. Hier ist ein Beispiel, wie man eine HTTP-Anforderung sendet und den Inhalt der Antwort druckt:

```Gleam
// Importiere die HTTP-Bibliothek
const http = import gleam/http

// Definiere die Authentifizierungsdaten
let username = "Benutzername"
let password = "Passwort"

// Sende eine GET-Anfrage mit Basic-Authentifizierung
let response = http.get("https://beispiel.com/meineDaten", |authorisation=Some(http.auth(username, password)))

// Drucke den Inhalt der Antwort auf der Konsole
pub fn main() {
  debug.response.body
}
```

Tiefer Einblick:

Die Basic-Authentifizierung ist eine Methode zur HTTP-Authentifizierung, die bereits seit den frühen Tagen des Internets verwendet wird. Es basiert auf dem Versenden von Benutzername und Passwort im Klartext, was jedoch aufgrund von Sicherheitslücken immer mehr durch andere Methoden ersetzt wird. Alternativen zur Basic-Authentifizierung sind z.B. die Digest-Authentifizierung oder OAuth.

In Gleam wird die Authentifizierung mit Basic-Authentifizierung durch die Funktion `http.auth` unterstützt, die einen String im HTTP-Basic-Auth-Format zurückgibt. Diese kann dann zusammen mit der HTTP-Anforderung an den Server gesendet werden. Es ist wichtig zu beachten, dass die Basic-Authentifizierung nicht als sichere Methode zur Datenübertragung betrachtet wird, da die Zugangsdaten im Klartext versendet werden.

Weitere Informationen:

- [Gleam HTTP-Bibliothek Dokumentation](https://gleam.run/modules/gleam_http/latest)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Alternative Methoden zur HTTP-Authentifizierung](https://www.baeldung.com/spring-security-choose-authentication-protocol)