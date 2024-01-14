---
title:                "Gleam: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung kann in vielen Fällen nützlich sein, wie z.B. beim Zugriff auf geschützte Ressourcen oder bei der Überprüfung von Benutzeranmeldungen.

## Wie funktioniert es?

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, muss zunächst ein Authentifizierungs-Header mit den Zugangsdaten des Benutzers erstellt werden. Hier ist ein Beispiel, wie dies mit Gleam gemacht werden kann:

```Gleam
let url = "https://example.com/api"
let username = "example"
let password = "password"

let request = Http.basic_auth_request(url, username, password)
let response = Http.fetch(request)

# Beispiel-Ausgabe:
Statuscode: 200
Body: {"message": "Authentifizierung erfolgreich!"}
``` 

## Tiefere Einblicke

Kommt es zu einer fehlgeschlagenen Authentifizierung, wird meist ein Statuscode von 401 zurückgegeben. Es ist auch möglich, zusätzliche Optionen wie z.B. die Verschlüsselung der Anfrage hinzuzufügen. Weitere Informationen dazu findet man in der Gleam-Dokumentation zu `Http.basic_auth_request`.

# Siehe auch

- Gleam-Dokumentation: https://gleam.run/articles/http/
- HTTP-Statuscodes erklärt: https://developer.mozilla.org/de/docs/Web/HTTP/Status
- Grundlagen der Authentifizierung: https://www.lifewire.com/what-is-basic-access-authentication-817131