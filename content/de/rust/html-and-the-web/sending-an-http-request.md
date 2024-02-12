---
title:                "Einen HTTP-Request senden"
aliases:
- /de/rust/sending-an-http-request/
date:                  2024-01-20T18:00:41.912840-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind der Austausch von Daten über das Internet nach dem Client-Server-Modell. Programmierer verwenden sie, um mit Webdiensten zu interagieren, Daten abzurufen oder zu senden – etwa, um Web-APIs zu nutzen oder Server-Status zu überprüfen.

## How to:
Um in Rust eine HTTP-Anfrage zu senden, nutzen wir die `reqwest`-Bibliothek. Hier ist ein einfaches Beispiel, wie man eine GET-Anfrage absendet:

```Rust
use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let response = reqwest::get("https://www.rust-lang.org")
        .await?
        .text()
        .await?;

    println!("Body:\n{}", response);
    Ok(())
}
```

Ausgabe:

```
Body:
<!doctype html>
...
```

Für POST-Anfragen sieht das ähnlich aus, mit zusätzlichen Daten im Body:

```Rust
use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let res = client.post("http://httpbin.org/post")
        .body("key=value")
        .send()
        .await?;

    println!("Status: {}", res.status());
    Ok(())
}
```

Ausgabe:

```
Status: 200 OK
```

## Deep Dive:
Historisch gesehen basieren HTTP-Anfragen auf dem Hypertext Transfer Protocol, das 1991 eingeführt wurde. Rust bietet, wie die meisten modernen Programmiersprachen, Pakete, um diesen Prozess zu vereinfachen. Während `reqwest` für viele ein Go-to-Paket ist, gibt es Alternativen wie `hyper` für niedrigere Ebenen der Abstraktion oder `surf`, falls nur Async-STD verfügbar ist.

Die Implementierung einer HTTP-Anfrage in Rust erfordert eine asynchrone Umgebung, da Netzwerkanfragen I/O-blockierend sein können. Rust's `async/await` ermöglicht effiziente, nicht-blockierende Ausführung. `reqwest` nutzt `hyper` als HTTP-Implementierung, die wiederum auf `tokio` aufbaut, einem asynchronen Runtime.

## See Also:
- [`reqwest` Dokumentation](https://docs.rs/reqwest/)
- [Rust `async` Buch](https://rust-lang.github.io/async-book/)
- [HTTP in Rust mit `hyper`](https://hyper.rs/)
- [`surf` GitHub Repository](https://github.com/http-rs/surf)
