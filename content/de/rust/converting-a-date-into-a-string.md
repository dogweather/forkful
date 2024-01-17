---
title:                "Umwandeln eines Datums in eine Zeichenkette"
html_title:           "Rust: Umwandeln eines Datums in eine Zeichenkette"
simple_title:         "Umwandeln eines Datums in eine Zeichenkette"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was und warum?
Das Konvertieren eines Datums in einen String ist ein wichtiger Prozess in der Programmierung, bei dem ein Datumsobjekt in ein lesbare Textform umgewandelt wird. Dadurch können Daten einfacher dargestellt und gespeichert werden, was für die Verarbeitung und Analyse von Informationen entscheidend ist.

## So geht's:
```Rust

// Beispielcode zur Konvertierung eines Datums in einen String
use chrono::{DateTime, Utc, TimeZone};

fn main() {
   let date_time = Utc::now(); // Aktuelles Datum und Uhrzeit abrufen
   let date_string = date_time.format("%d.%m.%Y %H:%M:%S").to_string(); // Datumsformatierung und Konvertierung in einen String

   println!("Das aktuelle Datum und Uhrzeit ist: {}", date_string); // Ausgabe des Datums als String
}
```
Die Ausgabe des obigen Codes wäre in etwa so: "Das aktuelle Datum und Uhrzeit ist 15.09.2021 18:56:23".

## Tiefergehende Informationen:
Die Konvertierung eines Datums in einen String ist ein weit verbreitetes Verfahren, das in verschiedenen Programmiersprachen verwendet wird. In Rust wird das Modul "chrono" verwendet, um mit Datum und Uhrzeit zu arbeiten. Alternativ können auch andere Bibliotheken wie "time" oder "date_time" genutzt werden.

Die Umwandlung von Datumsobjekten in Strings kann auch in verschiedene Formate erfolgen, je nach den spezifischen Anforderungen des Programms. In unserem Beispiel haben wir das Datum im Format "Tag.Monat.Jahr Stunden:Minuten:Sekunden" angegeben, aber es gibt viele andere Möglichkeiten, wie z.B. die Formatierung als ISO-Standard oder im 12-Stunden-Format.

Die Implementation in Rust ist einfach und effektiv, da das Modul "chrono" eine Vielzahl von Funktionen und Methoden bietet, um mit Datum und Uhrzeit zu arbeiten. Die Dokumentation des Moduls ist umfangreich und bietet eine detaillierte Erklärung aller verfügbaren Funktionen.

## Siehe auch:
- [Offizielle Dokumentation von Rust zum Modul "chrono"](https://docs.rs/chrono/0.4.19/chrono/)
- [Zusammenfassung von Datum und Uhrzeit in Rust](https://www.rust-lang.org/learn/datetime)
- [Berechnungen mit Datum und Uhrzeit in Rust](https://medium.com/better-programming/handling-dates-and-times-in-rust-7f55ed357720)