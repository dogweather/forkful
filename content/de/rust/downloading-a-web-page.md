---
title:                "Herunterladen einer Webseite"
html_title:           "Rust: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir uns mit dem Wie und dem Was beschäftigen, lassen Sie uns kurz darüber sprechen, warum Sie überhaupt eine Webseite herunterladen möchten. Vielleicht sind Sie ein Entwickler, der an einem Webcrawler arbeitet, der die Inhalte einer Webseite analysiert. Oder Sie möchten einfach eine lokale Kopie einer Webseite für den Offline-Zugriff speichern. Ganz gleich aus welchem Grund, das Herunterladen von Webseiten kann eine nützliche Fähigkeit sein, die Sie in Ihrer Programmierungsreise beherrschen sollten.

## Wie geht's?

Um eine Webseite herunterzuladen, benötigen wir zuerst das "reqwest" Paket, das es uns ermöglicht, HTTP-Anfragen zu senden und Antworten zu empfangen. Stellen Sie sicher, dass Sie es in Ihrer Datei "Cargo.toml" aufgelistet haben.

Als nächstes definieren wir die URL der Webseite, die wir herunterladen möchten, und speichern sie in einer Variablen. Dann erstellen wir ein "Client" Objekt und führen eine GET-Anfrage an die angegebene URL aus.

```Rust
let url = "https://www.beispielwebseite.de/";
let client = reqwest::Client::new();
let res = client.get(url).send().await?;
```

Dieser Code verwendet die asynchrone Programmierungsfunktion von Rust (das `await` Schlüsselwort), das ist jedoch nicht unbedingt erforderlich. Um die Antwort der Anfrage zu erhalten, können wir die Funktion `text()` auf dem `res` Objekt aufrufen.

```Rust
let body = res.text().await?;
```

Danach können wir die erhaltenen Daten einfach in eine Datei schreiben oder weiterverarbeiten, wie wir es benötigen.

## Tiefer einsteigen

Die "reqwest" Dokumentation bietet viele nützliche Funktionen, um mit HTTP-Anfragen und -Antworten zu interagieren. Es gibt auch weitere Pakete wie "scraper" und "hyper" für fortgeschrittenere Web-Scraping-Aufgaben. Darüber hinaus gibt es zahlreiche Möglichkeiten, mit HTTP in Rust zu arbeiten, einschließlich der Unterstützung von asynchroner Programmierung, Verschlüsselung und vielem mehr. Die offizielle Rust-Website bietet weiterführende Informationen und Ressourcen für diejenigen, die sich für tiefergehende Informationen interessieren.

## Siehe auch

- [Rust](https://www.rust-lang.org/)
- [reqwest Dokumentation](https://docs.rs/reqwest/latest/reqwest/)
- [scraper Paket](https://docs.rs/scraper/latest/scraper/)
- [hyper Paket](https://docs.rs/hyper/latest/hyper/)