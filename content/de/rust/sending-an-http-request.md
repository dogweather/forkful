---
title:                "Senden einer http Anfrage"
html_title:           "Rust: Senden einer http Anfrage"
simple_title:         "Senden einer http Anfrage"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Senden von HTTP-Anfragen handelt es sich um die Kommunikation zwischen einem Client und einem Server über das Hypertext Transfer Protocol (HTTP). Programmierer tun dies, um auf Ressourcen oder Daten auf einem Server zuzugreifen, um sie zu nutzen oder zu manipulieren.

## Wie geht's?
Hier ist ein Beispiel, wie man eine HTTP-Anfrage in Rust sendet:

```Rust
use reqwest;

async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let response = reqwest::get("https://www.example.com")
        .await?
        .text()
        .await?;

    println!("{}", response);
    Ok(())
}
```

Die Ausgabe des Codes ist der HTML-Inhalt der Website "www.example.com".

## Tiefere Einblicke
Das Senden von HTTP-Anfragen hat eine lange Geschichte und ist ein grundlegender Bestandteil der heutigen Web-Technologie. Es gibt auch Alternativen zum Versenden von HTTP-Anfragen, wie z.B. die Verwendung von Datenaustauschprotokollen wie JSON oder XML. Die Implementierung für das Senden von HTTP-Anfragen in Rust wird durch Bibliotheken wie reqwest erleichtert, die die Funktionalität vereinfachen und die Schreibarbeit reduzieren.

## Sieh dir auch an
Für weitere Informationen zum Senden von HTTP-Anfragen in Rust, schau dir die offizielle Dokumentation von reqwest an: https://docs.rs/reqwest.