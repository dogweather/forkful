---
title:    "Rust: Das aktuelle Datum erhalten"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist ein wichtiger Bestandteil vieler Anwendungen, da es häufig verwendet wird, um Zeitstempel zu generieren oder Daten zu organisieren. In diesem Blogbeitrag werden wir uns ansehen, wie man das aktuelle Datum in der Programmiersprache Rust abrufen kann.

## How To

Um das aktuelle Datum in Rust abzurufen, können wir die Standardbibliotheksfunktion `Local::now()` verwenden. Dies gibt uns ein `LocalDateTime`-Objekt zurück, das wir dann in das gewünschte Format konvertieren können.

```Rust
use chrono::Local;
let now = Local::now(); // aktuelles Datum abrufen
let formatted_date = now.format("%d.%m.%Y").to_string(); // in das Format "Tag.Monat.Jahr" konvertieren
println!("Das aktuelle Datum ist: {}", formatted_date); // Ausgabe: Das aktuelle Datum ist: 07.10.2021
```

Wir können diese Methode auch verwenden, um die aktuelle Uhrzeit oder Zeitzone abzurufen, indem wir das entsprechende Format in `format()` angeben.

## Deep Dive

Das `LocalDateTime`-Objekt, das `Local::now()` zurückgibt, ist Teil der externen Bibliothek `chrono`, die eine Vielzahl von Datums- und Uhrzeitmanipulationen in Rust ermöglicht. Dieses Objekt verwendet auch verschiedene Zeitzonen, um sicherzustellen, dass die erhaltenen Daten lokal korrekt sind.

Es ist auch wichtig zu beachten, dass das aktuelle Datum vom System abhängt, auf dem die Anwendung ausgeführt wird. Das bedeutet, dass das Datum möglicherweise nicht genau ist, wenn die Systemzeit nicht richtig eingestellt ist.

## Siehe auch

- [Rust Standardbibliothek](https://doc.rust-lang.org/std/)
- [Chrono Dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Programmierhandbuch](https://doc.rust-lang.org/book/)