---
title:                "Rust: Senden eines http-Anforderung mit grundlegender Authentifizierung"
simple_title:         "Senden eines http-Anforderung mit grundlegender Authentifizierung"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Senden von HTTP-Anfragen mit grundlegender Authentifizierung befassen? Nun, in der heutigen digitalen Welt sind HTTP-Anfragen eine häufige Methode, um mit Webdiensten zu kommunizieren. Oftmals erfordern diese Dienste eine Authentifizierung, um sicherzustellen, dass nur berechtigte Benutzer Zugriff auf sensible Informationen haben. Deshalb ist es wichtig zu verstehen, wie man solche HTTP-Anfragen mit grundlegender Authentifizierung sendet.

## Wie man es macht

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Rust zu senden, müssen wir zunächst die notwendigen Abhängigkeiten importieren. Das können wir im `Cargo.toml`-File unseres Projekts tun.

```Rust
[dependencies]
reqwest = "0.11.4"
base64 = "0.11.0"
```

Dann können wir die `reqwest`-Library verwenden, um unsere Anfrage zu erstellen und zu senden. Wir werden auch die `base64`-Library benötigen, um unser Benutzername und Passwort in base64 zu kodieren.

```Rust
use reqwest::{Client, Response};
use base64::encode;
```

Als nächstes müssen wir unsere Anfrage-URL und unsere Zugangsdaten festlegen, die wir in base64 kodieren werden.

```Rust
let url = "https://example.com/api/users";
let username = "meinname";
let password = "meinpasswort";
let auth = format!("{}:{}", username, password);
let basic_auth = encode(auth.as_bytes());
```

Dann können wir unsere Anfrage erstellen und die HTTP-Authentifizierung hinzufügen.

```Rust
let client = Client::new();
let response = client
    .get(url)
    .header("Authorization", format!("Basic {}", basic_auth))
    .send()
    .await?;
```

Schließlich können wir die Antwort ausgeben, um sicherzustellen, dass unsere Anfrage erfolgreich gesendet wurde.

```Rust
println!("{:?}", response.text().await?);
```

Wenn wir diesen Code ausführen, sollten wir eine Antwort mit den gewünschten Daten erhalten.

## Tiefgehende Analyse

Wie Sie sehen können, ist es ziemlich einfach, in Rust eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden. Es mag zwar etwas komplexer erscheinen als in anderen Programmiersprachen, aber es bietet auch einige Vorteile. Zum Beispiel ist Rust eine schnellere und sicherere Sprache, die dafür sorgt, dass unsere Anfragen zuverlässig und effizient gesendet werden.

Es ist auch wichtig zu beachten, dass es verschiedene Arten der Authentifizierung gibt, die bei HTTP-Anfragen verwendet werden können, und dass die grundlegende Authentifizierung nicht immer die sicherste ist. Es ist ratsam, sich mit anderen Methoden wie OAuth oder Token-Authentifizierung vertraut zu machen, je nach den Anforderungen und Sicherheitsbedenken Ihres Projekts.

## Siehe auch

- [Rust Dokumentation](https://www.rust-lang.org/learn)
- [Offizielle reqwest Dokumentation](https://docs.rs/reqwest/0.11.4/reqwest/)
- [Weitere Beispiele für HTTP-Anfragen mit Rust](https://blog.logrocket.com/how-to-send-http-requests-in-rust/)