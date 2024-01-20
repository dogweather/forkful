---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP-Anforderung (HTTP Request) senden bedeutet, eine Anfrage an einen Server zu stellen, um Informationen zu empfangen oder zu senden. Programmierer tun dies, um Daten von einer Web-API zu holen oder diese zu manipulieren.

## Wie geht das?

Wir verwenden das `reqwest` Paket, um HTTP-Anforderungen in Rust zu senden. Fügen Sie zuerst `reqwest` zu Ihren Abhängigkeiten hinzu.

```Rust
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Jetzt können wir eine GET-Anfrage an eine API senden.

```Rust
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let resp = reqwest::get("https://httpbin.org/get").await?;

    println!("{}", resp.text().await?);
    Ok(())
}
```

Ausführung des obigen Codes produziert einen Output wie:

```Rust
{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    ... 
  }, 
  "url": "https://httpbin.org/get"
}
```

## Vertiefung

Das Versenden einer HTTP-Anforderung ist ein grundlegender Bestandteil des Internets. Historisch gesehen hat Rust dies durch die Standardbibliothek ermöglicht. Ähnlich wie viele andere Sprachen hat Rust jedoch Pakete entwickelt, um den Prozess zu vereinfachen und mehr Funktionen zu ermöglichen.

Alternativen zu `reqwest` sind unter anderem `hyper` und `surf`. Die Verwendung dieser Pakete hängt von den Anforderungen Ihres Projekts ab.

Die Implementierung von HTTP-Anforderungen mit `reqwest` ist ziemlich einfach. `reqwest` nutzt `tokio` für asynchrone I/O, was das Senden von HTTP-Anforderungen erleichtert.

## Siehe auch

Weitere Informationen und Beispiele für HTTP-Anforderungen in Rust finden Sie in den offiziellen `reqwest` [Dokumentation](https://docs.rs/reqwest/) und [GitHub Repository](https://github.com/seanmonstar/reqwest).

Falls Sie an den tieferen Details von asynchronem I/O in Rust interessiert sind, schauen Sie sich das `tokio` [Dokumentation](https://docs.rs/tokio/1.0.1/tokio/).