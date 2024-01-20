---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-Anfragen mit Basic Authentication senden in Rust

## Was & Warum?

Das Senden einer HTTP-Anfrage mit Basic Authentication bezieht sich auf den Prozess, einen verschlüsselten Benutzernamen und Passwort mit einer Anfrage zu senden, um die Identität des Anwenders zu bestätigen. Programmierer machen dies, um sicherzustellen, dass nur authentifizierte Nutzer auf bestimmte Daten oder Funktionen zugreifen können.

## Wie es geht:

Um in Rust eine HTTP-Anfrage mit Basic Authentication zu senden, verwenden wir die Bibliothek reqwest. Zuerst müssen wir sie mit ```cargo add reqwest``` in unser Projekt aufnehmen.

```Rust
extern crate reqwest;
use reqwest::header::{HeaderValue, AUTHORIZATION};

let client = reqwest::Client::new();
let auth = format!("Basic {}", base64::encode("Benutzername:Passwort"));
let res = client.get("https://api.deineWebsite.com/daten")
    .header(AUTHORIZATION, HeaderValue::from_str(&auth).unwrap())
    .send()
    .await?;

println!("Status: {}", res.status());
let text = res.text().await?;
println!("Body: \n\n{}", text);
```

In diesem Code wird der Benutzername und das Passwort mit Basic Authentication an die URL gesendet, die du angeben. 

## Deep Dive

Historisch gesehen ist die Basic Authentication eine der ursprünglichsten Methoden zum Authentifizieren von HTTP-Anfragen und war bereits im ersten HTTP-Standard vorgesehen. Es hat seine Schwächen (wie die Übertragung von unverschlüsselten Passwörtern), aber es bleibt immer noch eine einfache und weit unterstützte Option.

Alternativen zur Basic Authentication beinhalten OAuth, das einen dritten Parteien ermöglicht, ohne ein Passwort auf Nutzerdaten zuzugreifen, oder Digest Authentication, mit der Passwörter verschlüsselt an den Server gesendet werden.

In Rust wird die Basic Authentication normalerweise über die HeaderProperties implementiert. Das `reqwest`-Package verwendet eine HeaderMap, um HeaderProperties zu speichern. Authentifizierungsinformationen werden dem "Authorization"-Header als eine HeaderValue hinzugefügt, die den Benutzernamen und das Passwort enthält. Dann wird die Anfrage mit diesem Header gesendet.

## Siehe auch

- [reqwest Dokumentation](https://docs.rs/reqwest)
- [Basic Authentication auf MDN](https://developer.mozilla.org/de/docs/Web/HTTP/Headers/Authorization)