---
title:    "Rust: Ein Datum in eine Zeichenkette umwandeln."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum 

Das Umwandeln von Daten in eine Zeichenfolge ist eine gängige Aufgabe in der Programmierung. Es kann hilfreich sein, Daten in einer lesbaren Form anzuzeigen oder sie für die Ausgabe in einem bestimmten Format vorzubereiten.

## Wie man

Um ein Datum in Rust in eine Zeichenfolge umzuwandeln, können wir die `format!` Macro verwenden. Hier ist ein Beispiel, das ein Datum in das Format "Tag.Monat.Jahr" umwandelt:

```Rust 
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let seconds_since_epoch = now.duration_since(UNIX_EPOCH).unwrap().as_secs();

let date = UNIX_EPOCH + Duration::from_secs(seconds_since_epoch);

let formatted_date = format!("{}", date.format("%d.%m.%Y"));
```

Die Ausgabe des oben genannten Codes würde ähnlich aussehen wie: "12.04.2021". Durch die Verwendung der `format!` Macro können wir auch angeben, welches Format wir für die Ausgabe verwenden möchten.

## Tief eintauchen 

Wenn wir uns die `format!` Macro genauer ansehen, können wir sehen, dass sie tatsächlich eine Implementierung der `std::fmt::Display` Trait verwendet. Dieser Trait ist für die Anzeige von Daten verantwortlich und wird verwendet, um eine formatierte Zeichenfolge zurückzugeben. Die `format!` Macro ist also eine bequeme Möglichkeit, ein Datum in eine Zeichenfolge umzuwandeln, aber sie basiert auf den Konzepten des `Display` Traits.

## Siehe auch

- [Rust Standardbibliothek Dokumentation](https://doc.rust-lang.org/std/time/struct.Format.html)
- [Weitere Beispiele zur Konvertierung von Daten in Zeichenfolgen in Rust](https://www.geeksforgeeks.org/formatting-macro-in-rust-format/)