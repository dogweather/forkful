---
title:                "Rust: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datumswerten in Strings ist ein wichtiger Aspekt der Programmierung, vor allem wenn es darum geht, Daten auf eine für Benutzer lesbarere Weise darzustellen. In Rust bietet die Standardbibliothek einige nützliche Methoden, um diese Aufgabe zu bewältigen. In diesem Beitrag werden wir uns genauer ansehen, wie man ein Datum in einen String umwandelt und einige Tipps und Tricks dazu geben.

## Wie man ein Datum in einen String umwandelt

Die Methode `.to_string` kann verwendet werden, um ein Datum in einen String umzuwandeln. Dazu muss jedoch zuerst das Datum in ein `DateTime `-Objekt umgewandelt werden. Das kann mithilfe der `chrono`-Bibliothek von Rust erfolgen. Betrachten wir folgendes Beispiel:

```rust
extern crate chrono;

use chrono:DateTime;
use chrono::Utc;

fn main() {
    let date = Utc::now();
    let datetime = DateTime::<Utc>::from(date);
    let datestring = datetime.to_string();
    println!("{}", datestring);
}
```

Der Output dieses Codes wäre der folgende String: `2021-03-07 20:00:00 UTC`. Hier haben wir das aktuelle Datum mithilfe des `Utc::now`-Methodenaufrufs erhalten und es dann in ein `DateTime`-Objekt umgewandelt. Danach wurde die `.to_string`-Methode aufgerufen, um das Datum in einen String zu konvertieren. Dieser String kann dann für verschiedene Zwecke verwendet werden, wie zum Beispiel die Anzeige auf einer Benutzeroberfläche.

## Tiefergehende Informationen zum Konvertieren von Datum in ein String

Das oben gezeigte Beispiel ist nur eine einfache Möglichkeit, ein Datum in einen String umzuwandeln. In der `chrono`-Bibliothek gibt es einige weitere nützliche Methoden, um die Konvertierung zu erleichtern. Zum Beispiel kann die `format`-Methode verwendet werden, um das Datum in einem bestimmten Format darzustellen. Außerdem bietet Rust auch die Möglichkeit, benutzerdefinierte Formate zu erstellen. Weitere Informationen dazu können in der Dokumentation zur `chrono`-Bibliothek gefunden werden.

## Siehe auch

- [Rust Standardbibliothek](https://doc.rust-lang.org/std/)
- [Chrono-Dokumentation](https://docs.rs/chrono/0.4.19/chrono/)