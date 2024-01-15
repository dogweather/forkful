---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Rust: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit der Konvertierung eines Datums in einen String beschäftigen? Ganz einfach, weil dies eine häufige Aufgabe in der Programmierung ist. Eine Reihe von Anwendungen erfordern die Darstellung von Daten in einem bestimmten Format, sei es für die Benutzeroberfläche oder für den Austausch mit anderen Systemen.

## Wie geht das?
Um ein Datum in einen String umzuwandeln, gibt es in Rust die Funktion `to_string()` in der `chrono` Bibliothek. Schauen wir uns an, wie wir diese Funktion verwenden können:
```Rust
use chrono::{DateTime, Local, TimeZone};

// Aktuelles Datum und Uhrzeit erstellen
let now: DateTime<Local> = Local::now();

// Datum in String konvertieren
let date_string = now.to_string();

// Output: 2021-10-05 19:30:00
println!("{}", date_string);
```
Mit `to_string()` können wir also bequem ein Datum in einem Standardformat konvertieren. Aber was ist, wenn wir ein spezifisches Format benötigen? Auch hier bietet `chrono` verschiedene Funktionen an, um Datum und Uhrzeit nach unseren Bedürfnissen zu formatieren. Schauen wir uns ein Beispiel an:
```Rust
use chrono::{DateTime, Local, TimeZone, Datelike, Timelike};

// Aktuelles Datum und Uhrzeit erstellen
let now: DateTime<Local> = Local::now();

// Datum im Format "DD.MM.YYYY" konvertieren
let date_string = format!("{}.{}.{}", now.day(), now.month(), now.year());

// Output: 05.10.2021
println!("{}", date_string);

// Uhrzeit im 24-Stunden-Format konvertieren
let time_string = format!("{}:{}", now.hour(), now.minute());

// Output: 19:30
println!("{}", time_string);
```

## Tiefergehende Informationen
Die `chrono` Bibliothek basiert auf der `DateTime` Struktur, die es uns ermöglicht, mit Datum und Uhrzeit in einer performanten und benutzerfreundlichen Art und Weise zu arbeiten. Es gibt auch weitere Funktionen wie `date()` und `time()`, die es uns ermöglichen, nur Teile eines `DateTime` Objekts zu extrahieren. Um mehr über die `chrono` Bibliothek zu erfahren, empfehle ich einen Blick in die offizielle Dokumentation.

## Siehe auch
- [Chrono-Dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust-Dokumentation](https://www.rust-lang.org/de/learn)