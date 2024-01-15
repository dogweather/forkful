---
title:                "Das Senden einer HTTP-Anfrage"
html_title:           "Rust: Das Senden einer HTTP-Anfrage"
simple_title:         "Das Senden einer HTTP-Anfrage"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals eine Webseite besucht haben, haben Sie auch wahrscheinlich eine HTTP-Anfrage gesendet, ohne es zu merken. Aber hast du dich jemals gefragt, wie das eigentlich funktioniert und wie du in Rust selbst eine HTTP-Anfrage senden könntest? In diesem Artikel werden wir genau das besprechen.

## Wie Geht man Vor

Um eine HTTP-Anfrage in Rust zu senden, müssen Sie zuerst das `reqwest`-Paket in Ihrer Cargo.toml-Datei hinzufügen. Dann können Sie die Bibliothek in Ihrem Code importieren und eine Anfrage an eine beliebige URL senden.

```rust
use reqwest::Error;

async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://www.example.com").await?;

    println!("Status code: {}", response.status());
    println!("Body: {}", response.text().await?);

    Ok(())
}
```

Dieses Beispiel sendet eine einfache GET-Anfrage an die `example.com`-Website und gibt den Statuscode und den Text der Antwort auf der Konsole aus.

## Tief Tauchen

Wenn wir uns genauer ansehen, was hinter den Kulissen passiert, wenn wir eine HTTP-Anfrage senden, sehen wir, dass es verschiedene Arten von Anfragemethoden gibt, wie GET, POST, PUT und DELETE. Wir können auch Parameter und Header zu unserer Anfrage hinzufügen, um bestimmte Ergebnisse zu erhalten.

Um beispielsweise eine POST-Anfrage mit JSON-Körper und benutzerdefinierten Headern zu senden, können wir den folgenden Code verwenden:

```rust
use reqwest::{Client, Error};

fn main() -> Result<(), Error> {
    let client = Client::new();

    let response = client.post("https://www.example.com")
        .header("Authorization", "Bearer TOKEN")
        .header("Content-Type", "application/json")
        .json(&json!({
            "name": "John Doe",
            "email": "john@example.com"
        }))
        .send()?;

    println!("Status code: {}", response.status());
    println!("Body: {}", response.text().await?);

    Ok(())
}
```

Hier verwenden wir die `Client`-Struktur, um unsere Anfrage zu konfigurieren und zu senden. Wir fügen auch benutzerdefinierte Header hinzu und wandeln unseren JSON-Körper in ein `serde_json`-Objekt um, damit er von der Bibliothek verarbeitet werden kann.

## Siehe Auch

- [Offizielle Dokumentation von reqwest](https://docs.rs/reqwest/latest/reqwest/)
- [HTTP in Rust mit Hyper](https://blog.logrocket.com/http-in-rust-with-hyper/)
- [Das ultimative Rust Web Framework Shootout](https://www.lpalmieri.com/posts/2020-08-01-performance-shootout-warp-hyper-actix-web/)