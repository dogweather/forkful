---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Datum in Rust abrufen: Warum, wie und mehr

Rust ist eine moderne Programmiersprache, die auf Leistung, Zuverlässigkeit und produktive Entwicklung ausgerichtet ist. In diesem Artikel werden wir uns darauf konzentrieren, wie wir in Rust das aktuelle Datum abrufen.

## Was & Warum?

Das Abrufen des aktuellen Datums ist das Erfassen des aktuellen Tags, Monats und Jahres. Dies ist äußerst nützlich für die Protokollierung von Ereignissen, das Festlegen von Zeitstempeln und das Verfolgen von Zeitfenstern in einer Anwendung.

## So geht's:

Rust bietet mit dem Modul `chrono` eine einfache Möglichkeit, das aktuelle Datum zu erhalten. Hier ist ein Codebeispiel:

```Rust
use chrono::{Date, Local};

fn main() {
    let today: Date<Local> = Local::today();
    println!("{}", today.format("%d-%m-%Y").to_string());
}
```

Wenn Sie dieses Programm ausführen, erhalten Sie das aktuelle Datum im Format dd-mm-yyyy.

## Deep Dive:

### Historischer Kontext:
Chrono ist das Standardmodul zum Arbeiten mit Datum und Zeit in Rust. Es wurde entwickelt, um die Komplexität der Datums- und Zeitverarbeitung in anderen Sprachen zu beseitigen.

### Alternativen:
Es gibt auch Alternativen wie `time` Modul, die zum Abrufen des aktuellen Datums in Rust verwendet werden können. 

### Implementierungsdetails:
Die `chrono`-Bibliothek bietet Zeitzonen-unterstützte, genaue und bequeme Datums- und Zeitfunktionen. Die Funktion `Local::today()` gibt das aktuelle Datum als `Date<Local>` zurück.

## Siehe auch:

Weitere Informationen finden Sie in der offiziellen `chrono`-Dokumentation: [Chrono auf crates.io](https://docs.rs/chrono/0.4.11/chrono/)
Für eine detailliertere Anleitung zur Verwendung von `chrono` schauen Sie hier: [Offizielles Chrono Tutorial](https://lifthrasiir.github.io/rustlog/why-is-chrono-a-modern-date-and-time-library.html)

Viel Spaß beim Programmieren mit Rust!