---
title:                "Rust: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Rust ist eine moderne Programmiersprache, die sich auf Performance und Sicherheit fokussiert. Mit ihr kannst du schnell und zuverlässig Webseiten herunterladen und verarbeiten.

## Wie funktioniert es

Um mit Rust eine Webseite herunterzuladen, benötigst du die Bibliothek `reqwest`. Diese kannst du einfach mit Cargo installieren. Dann musst du nur noch folgenden Code schreiben:

```
Rust
use reqwest::get;

fn main() {
    let response = get("https://www.example.com").unwrap();

    // Get status code
    println!("Status code: {}", response.status());

    // Get response body as string
    let body = response.text().unwrap();
    println!("Response body: {}", body);
}

```

Dieser Code verwendet die Funktion `get()` aus der `reqwest`-Bibliothek, um eine GET-Anfrage an die angegebene URL zu senden. Die Antwort wird dann in der Variable `response` gespeichert. Durch die Verwendung von `unwrap` können wir auf die Statuscodes und den Inhalt der Antwort zugreifen.

Die Ausgabe des obigen Codes sollte folgendermaßen aussehen:

```
Status code: 200 OK
Response body: <html>
    <head>
        <title>Example Domain</title>
    </head>
    <body>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this
            domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </body>
</html>
```

## Tiefergehende Einblicke

Um fortgeschrittene Operationen wie das Verarbeiten von Header-Informationen oder das Speichern der erhaltenen Datei zu ermöglichen, bietet die `reqwest`-Bibliothek weitergehende Funktionen und Eigenschaften. Zudem ist sie leicht erweiterbar und kann beispielsweise durch das Hinzufügen von Proxies oder Benutzeragenten angepasst werden.

Es gibt auch andere Bibliotheken für die Verarbeitung von HTTP-Anfragen, aber `reqwest` gehört zu den beliebtesten und zuverlässigsten Optionen. Sie wird regelmäßig aktualisiert und unterstützt neue Funktionen und Protokolle.

## Siehe auch

- Offizielle Dokumentation von `reqwest` (https://docs.rs/reqwest/0.10.7/reqwest/)
- Weitere Informationen zu Rust (https://www.rust-lang.org/de/)

Vielen Dank, dass du diesen Artikel gelesen hast. Probiere es doch mal aus und lade deine Lieblingswebseite mit Rust herunter!