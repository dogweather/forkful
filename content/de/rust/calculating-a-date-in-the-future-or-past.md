---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Rust: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines zukünftigen oder vergangenen Datums ist eine Funktion, die ein bestimmtes Datum hinzuzufügt oder subtrahiert wird. Als Programmierer machen wir dies oft, um Zeitabläufe zu verfolgen oder Fristen zu berechnen.

## So geht's:

Im Rust-Programmiersprache können wir das Bibliothekspaket `chrono` verwenden, um diese Funktion einfach zu implementieren. Wenn du 'chrono' noch nicht hinzugefügt hast, füge es in dein 'Cargo.toml':

```Rust
[dependencies]
chrono = "0.4.19"
```

Hier ist ein einfacher Codeblock, der zeigt, wie du einem Datum eine Anzahl von Tagen hinzufügen kannst:

```Rust
use chrono::{Date, Utc, Duration};

fn main() {
    let today: Date<Utc> = Utc::today();
    
    let next_week = today.checked_add_signed(Duration::days(7)).unwrap();
    
    println!("{}", next_week);
}
```

Wenn du dieses Programm laufen lässt, wird es das Datum der nächsten Woche ausgeben.

## Tiefgehende Analyse

Die Berechnung von zukünftigen oder vergangenen Daten hat eine lange Geschichte in der Informatik und ihre Anwendungen sind weitreichend. Im Kontext von 'chrono', eine Bibliothek in Rust, könnten alternative Methoden die Verwendung von `checked_add_signed` oder `checked_sub_signed` sein, die eine Option zurückgeben, falls das Ergebnis außerhalb des gültigen Bereichs liegt.

Hinsichtlich der Implementierungsdetails verwendet Rust einen `i64`-Datentype zur Darstellung von Datums- und Zeitwerten. Dies ermöglicht eine präzise Berechnung und Behandlung von Zeiten, auch über sehr große Zeitspannen hinweg.

## Siehe auch

Mehr lernen kannst du aus den folgenden Ressourcen:

1. Chrono Documentation: [https://docs.rs/chrono/0.4.11/chrono/]
2. Rust Documentation: [https://doc.rust-lang.org/std/]
3. Rust Date and Time Tutorial: [https://www.tutorialspoint.com/rust/rust_date_time.htm]