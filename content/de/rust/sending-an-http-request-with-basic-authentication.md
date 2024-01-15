---
title:                "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "Rust: Versenden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man eine HTTP-Anfrage mit basic authentication senden? Nun, die Antwort ist einfach: Basic Authentication ist eine gängige Methode, um die Sicherheit von HTTP-Anfragen zu erhöhen. Indem man Benutzername und Passwort bei jeder Anfrage mitsendet, kann der Server prüfen, ob der Zugriff berechtigt ist.

## Wie geht's

Um eine HTTP-Anfrage mit basic authentication in Rust zu senden, verwenden wir die crate `reqwest`. Lass uns zunächst diese Crate importieren:

```Rust
use reqwest;
```

Jetzt können wir eine HTTP-Anfrage mit basic authentication senden, indem wir ein `Client`-Objekt erstellen und ihm die entsprechenden Zugangsdaten mitgeben:

```Rust
let client = reqwest::Client::builder()
    .basic_auth("username", Some("password"))
    .build()
    .unwrap();
```

Anschließend können wir die Anfrage an eine beliebige URL senden, zum Beispiel an `"https://www.example.com"`:

```Rust
let response = client
    .get("https://www.example.com")
    .send()
    .await
    .unwrap();
```

Dies wird eine `Response` zurückgeben, die wir nutzen können, um den Inhalt der Antwort auszugeben oder weiter zu verarbeiten:

```Rust
let body = response.text().await.unwrap();
println!("Antwort von www.example.com: {}", body);
```

Die Ausgabe würde in diesem Fall den HTML-Inhalt von `www.example.com` enthalten.

## Deep Dive

Wusstest du, dass basic authentication Teil des HTTP-Protokolls ist? Beim Senden einer Anfrage muss der `Authorization`-Header mit folgendem Format mitgesendet werden:

```
Authorization: Basic base64(username:password)
```

Dabei wird der Benutzername und das Passwort in klarer Textform aneinander gehängt und anschließend mit Base64 kodiert. Diese Methode ist zwar effektiv, um die Sicherheit von HTTP-Anfragen zu erhöhen, aber auch anfällig für Man-in-the-middle-Angriffe. Deshalb wird empfohlen, für eine höhere Sicherheit auf HTTPS zu setzen.

## Siehe auch

- Offizielle Dokumentation zu `reqwest`: https://docs.rs/reqwest/
- Informationen zu Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme
- RFC zu Basic Authentication: https://tools.ietf.org/html/rfc7617