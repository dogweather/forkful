---
title:    "Rust: Umwandeln eines Datums in einen String"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man in der Programmierung ein Datum in eine Zeichenkette umwandeln möchte. Zum Beispiel kann es hilfreich sein, um Daten in einem bestimmten Format anzuzeigen oder in einer Datenbank zu speichern. In diesem Blog-Beitrag werden wir lernen, wie man dies in Rust erreichen kann.

## Wie geht das?

Um ein Datum in eine Zeichenkette umzuwandeln, müssen wir die `format!` Funktion aus der `std::fmt` Bibliothek verwenden. Hier ist ein Beispielcode, der das aktuelle Datum in das Format "TT.MM.JJJJ" konvertiert:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};
use std::fmt::Display;

fn main() {
    let now = SystemTime::now();
    let unix_time = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
    let formatted_date = format!("{}", unix_time.as_secs());
    println!("{}", formatted_date);
}
```

Das Ergebnis dieses Codes sollte "TT.MM.JJJJ" sein.

Um das Datum in einem anderen Format anzuzeigen, können wir bestimmte Platzhalter in der `format!` Funktion verwenden. Zum Beispiel, um das Datum im Format "JJJJ-MM-TT" anzuzeigen, können wir folgenden Code verwenden:

```Rust
format!("{year}-{month}-{day}",
    year = unix_time.tm_year + 1900,
    month = unix_time.tm_mon + 1,
    day = unix_time.tm_mday
)
```

Das Ergebnis würde dann "JJJJ-MM-TT" sein.

## Eine nähere Betrachtung

Die `fmt` Bibliothek bietet viele verschiedene Platzhalter, die wir verwenden können, um ein Datum in unsere gewünschte Zeichenkette umzuwandeln. Einige der am häufig verwendeten Platzhalter sind:

- `%Y`: Das Jahr mit vier Ziffern.
- `%m`: Der Monat als Zahl (01-12).
- `%d`: Der Tag im Monat als Zahl (01-31).
- `%H`: Die Stunde im 24-Stunden-Format.
- `%M`: Die Minute als Zahl (00-59).
- `%S`: Die Sekunde als Zahl (00-60).
- `%a`: Die abgekürzte Wochentagsbezeichnung (z.B. "Mon").
- `%b`: Der abgekürzte Monatsname (z.B. "Jan").
- `%j`: Der Tag im Jahr als Zahl (001-366).

Eine vollständige Liste aller verfügbaren Platzhalter und deren Verwendung finden Sie in der offiziellen Dokumentation der `std::fmt` Bibliothek.

## Siehe auch

- [Offizielle Dokumentation zur `std::fmt` Bibliothek](https://doc.rust-lang.org/std/fmt/)
- [Weitere Informationen zu Datum und Zeit in Rust](https://doc.rust-lang.org/std/time/)

Vielen Dank, dass Sie diesen Artikel gelesen haben! Wir hoffen, dass er Ihnen bei der Konvertierung von Datum in eine Zeichenkette in Rust geholfen hat. Verpassen Sie nicht unsere weiteren Blog-Beiträge, die Ihnen helfen werden, Ihre Rust-Fähigkeiten zu verbessern. Bis bald!