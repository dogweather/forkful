---
title:                "Rust: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal eine Webseite oder eine App benutzt haben, haben Sie wahrscheinlich auch schon eine HTTP-Anfrage gesendet. Mit der Programmiersprache Rust können Sie dies auf einfache und effiziente Weise erledigen. Lesen Sie weiter, um herauszufinden, warum es sich lohnt, sich mit dem Senden von HTTP-Anfragen in Rust zu beschäftigen.

## Wie geht das?

Um eine HTTP-Anfrage in Rust zu senden, benötigen Sie zunächst das Crate `reqwest`. Dieses können Sie in Ihrer `Cargo.toml` Datei hinzufügen. Dann können Sie mit folgendem Code eine Anfrage an eine bestimmte URL senden:

```Rust
use reqwest::Url;

// URL festlegen
let url = Url::parse("https://www.beispielseite.de")?;

// HTTP-Anfrage senden
let response = reqwest::get(url)
    .await?
    .text()
    .await?;
```

Dieser Code erstellt zunächst eine `Url` mit der gewünschten Adresse und sendet dann eine HTTP-GET-Anfrage an diese URL. Mit `.text()` können Sie den Text der Antwort auslesen. Das Crate `reqwest` bietet jedoch noch viele weitere Funktionen und Optionen, mit denen Sie Ihre Anfrage anpassen und die Antwort verarbeiten können.

## Tiefgehende Einblicke

Wenn Sie tiefer in die Materie des Sendens von HTTP-Anfragen in Rust eintauchen möchten, gibt es einige interessante Themen, denen Sie sich widmen können. Zum Beispiel können Sie herausfinden, wie Sie die Methode und den Body Ihrer Anfrage anpassen oder wie Sie mit HTTP-Authentifizierung umgehen. Sie können auch lernen, wie Sie asynchrones HTTP verwenden oder wie Sie mit mehreren Anfragen parallel arbeiten können.

## Siehe auch

- Offizielle Dokumentation zu `reqwest`: https://docs.rs/reqwest/
- Ein Tutorial zum Senden von HTTP-Anfragen in Rust: https://blog.logrocket.com/sending-http-requests-in-rust/
- Eine Liste mit hilfreichen Crates für die Verwendung von HTTP in Rust: https://github.com/savoirfairelinux/rust-http-crates