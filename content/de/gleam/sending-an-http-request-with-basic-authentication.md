---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen mit Basic-Authentifizierung dienen dazu, sicherzustellen, dass nur autorisierte Benutzer auf bestimmte Webressourcen zugreifen können. Es ist ein einfacher, aber sicherer Weg, um die Vertraulichkeit und Integrität der zwischen Client und Server ausgetauschten Daten zu gewährleisten.

## Wie es geht:

In Gleam können Sie eine HTTP-Anfrage mit Basic-Authentifizierung senden, indem Sie die Bibliothek `gleam/httpc` verwenden. Hier ist ein Beispiel:

```Gleam
import gleam/httpc
import gleam/http.{HttpClient}

fn main(args: List(String)) {
  let client = HttpClient.start_link().gleam_expect("Failed to start HTTP client")
  let request = httpc.get("https://example.com")
  |> httpc.basic_auth("username", "password")
  let _ = client.send(request)
  |> result.unwrap(should_crash = True)
}
```

In diesem Beispiel erstellt das Programm eine GET-Anforderung an `https://example.com` und fügt eine Basic-Authentifizierung mit dem gegebenen Benutzernamen und Passwort hinzu.

## Vertiefung

Die Basic-Authentifizierung ist eine der ältesten Methoden zur Authentifizierung von HTTP-Anfragen. Sie wurde erstmals 1996 in RFC 1945 vorgeschlagen. Trotz ihrer Einfachheit bietet sie einen bescheidenen Schutz gegen unautorisierten Zugriff, indem sie Benutzername und Passwort über Base64 codiert.

Es gibt jedoch andere Alternativen wie OAuth, das eine sicherere und flexiblere Authentifizierung bietet, indem es Tokens anstelle von Benutzerdaten verwendet. In manchen Fällen kann auch eine IP-basierte Authentifizierung ausreichend sein.

In Gleam wird die Basic-Authentifizierung durch die Funktion `httpc.basic_auth/3` implementiert. Unter der Haube fügt diese Funktion den 'Authorization'-Header zur HTTP-Anfrage hinzu und formatiert den Benutzernamen und das Passwort gemäß dem Basic-Authentifizierungs-Schema.

## Siehe auch

- Offizielle Gleam-Dokumentation: https://gleam.run/documentation/
- RFC 1945 (HTTP/1.0, einschließlich Basic-Authentifizierung): https://datatracker.ietf.org/doc/html/rfc1945
- Gleam `httpc` Quellcode (für eine eingehende Untersuchung): https://github.com/gleam-lang/httpc