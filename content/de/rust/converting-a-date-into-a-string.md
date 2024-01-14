---
title:                "Rust: Umwandlung eines Datums in einen String"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren eines Datums in einen String ist eine häufige Aufgabe in der Programmierung, insbesondere wenn es um die Darstellung von Datumsangaben in einem benutzerfreundlichen Format geht. In Rust können wir diese Aufgabe effizient und fehlerfrei mit wenigen Zeilen Code erledigen. In diesem Blogpost werden wir uns genauer ansehen, wie wir diese Konvertierung durchführen können.

## Wie geht das?

Um ein Datum in einen String zu konvertieren, müssen wir zunächst das Modul "DateTime" aus der externen Crate "chrono" importieren. Dann können wir die Funktion "format" verwenden, um das Datum in einem gewünschten Format auszugeben. Hier ist ein Beispielcode:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc::now();
    let formatted_date = date.format("%d.%m.%Y").to_string();
    println!("{}", formatted_date);
}
```
Dieser Code importiert das aktuelle Datum in UTC und gibt es im Format "Tag.Monat.Jahr" aus. Wir können das Format beliebig ändern, indem wir die spezifischen Zeichen wie "%d" für den Tag, "%m" für den Monat und "%Y" für das vollständige Jahr austauschen.

Die Ausgabe wird in diesem Fall "10.08.2021" sein.

## Tief eintauchen

Wenn wir genauer hinschauen, stellen wir fest, dass die Funktion "format" eine Instanz der DateTime-Struktur erwartet, die in diesem Fall das aktuelle Datum und die Zeit in UTC darstellt. Diese Struktur bietet viele Methoden, um das Datum und die Zeit an unsere Bedürfnisse anzupassen. Wir können auch andere Datums- und Zeitformate verwenden, wie z.B. "Local" für das aktuelle Datum und die Zeit in der aktuellen Zeitzone.

Es ist auch wichtig zu beachten, dass die Funktion "format" eine Reihe von Konfigurationsmöglichkeiten bietet, um das Ausgabeformat weiter anzupassen. Wir können z.B. die Sprache, das Zeitzonenformat und die Anzeige des Tages der Woche hinzufügen.

Mit diesen Möglichkeiten können wir sicherstellen, dass das konvertierte Datum in unserem Programm genau so dargestellt wird, wie wir es möchten.

## Siehe auch

- Die offizielle Dokumentation von Rust für das Modul "DateTime" in der Crate "chrono": https://docs.rs/chrono/0.4.19/chrono/
- Weitere Beispiele und Erklärungen zur Konvertierung von Datum in einen String in Rust: https://www.educative.io/blog/rust-date-time-tutorial

Vielen Dank fürs Lesen und viel Spaß beim Programmieren mit Rust!