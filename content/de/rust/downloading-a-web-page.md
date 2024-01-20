---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Webseiten herunterzuladen bedeutet, die gesamten Daten einer Webseite auf die Festplatte zu kopieren, um sie offline zu betrachten oder zu analysieren. Programmierer machen das, um die Seitenlayouts zu testen, Daten zu sammeln oder einfach Webinhalte zu archivieren.

## So geht's:
Hier ist ein einfaches Beispiel, wie man mithilfe der Rust-Bibliothek "Reqwest" eine Webseite herunterlädt.

```Rust
extern crate reqwest;

fn main() -> Result<(), reqwest::Error> {
    let body = reqwest::get("https://www.example.com")
        .await?
        .text()
        .await?;

    println!("body = {:?}", body);

    Ok(())
}
```
Das obige Programm sendet eine GET-Anforderung an "https://www.example.com", liest den Text und gibt ihn in der Konsole aus.

## Tiefgang
Obwohl wir diese Technik heute oft verwenden, war das Herunterladen von Webseiten historisch gesehen nicht immer üblich. In erster Linie erforderten ältere Internetverbindungen eine konstante Verbindung zum Netz, um auf Websites zugreifen zu können.

Es gibt eine Reihe von Alternativen zum Herunterladen von Webseiten. Einige bevorzugen Tools wie `wget` oder `curl`, während andere Bibliotheken wie `HttpClient` in .NET oder `Jsoup` in Java verwenden.

Die genauen Einzelheiten der Implementierung hängen von der verwendeteten Technologie ab. Bei Rust-Bibliothek "Reqwest" beispielsweise besteht der Hauptteil der Implementierung im Wesentlichen aus einer Reihe von asynchronen Operationen.  

## Siehe auch
Haben Sie Lust auf mehr? Schauen Sie sich diese Quellen an:
- [Offizielle Rust-Dokumentation](https://doc.rust-lang.org/book/)
- [Reqwest-Dokumentation](https://docs.rs/reqwest/)
- [Webseiten herunterladen mit Java](https://jsoup.org/)