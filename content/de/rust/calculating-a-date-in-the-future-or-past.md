---
title:    "Rust: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit ist eine nützliche Fähigkeit beim Programmieren. Sie ermöglicht es, gezielt auf bestimmte Zeitpunkte zuzugreifen und so die Funktionalität eines Programms zu verbessern.

## Wie man es macht

Das Berechnen von Datumsangaben in Rust ist relativ einfach. Zunächst müssen wir das `chrono`-Paket importieren, um auf die entsprechenden Funktionen zugreifen zu können:

```Rust
use chrono::{Duration, NaiveDate}; 
```

Dann können wir das gewünschte Datum definieren und eine Dauer (in Tagen) angeben, um die gewünschte Datumsangabe zu erhalten. Zum Beispiel, um das Datum des Tages nach morgen zu berechnen:

```Rust
let morgen = NaiveDate::from_ymd(2021, 10, 1); // Definiert das Datum von morgen
let zukunft = morgen + Duration::days(1); // Berechnet das Datum des Tages nach morgen
println!("Das Datum des Tages nach morgen ist: {}", zukunft); // Gibt das Ergebnis aus
```

Die Ausgabe sollte wie folgt aussehen:

```
Das Datum des Tages nach morgen ist: 2021-10-02
```

## Tiefer Einblick

Bei der Berechnung eines Datums müssen wir die verschiedenen Datumsformate und Zeitzonen beachten. Auf diese Weise können wir sicherstellen, dass die berechneten Datumsangaben korrekt sind.

Es ist auch wichtig zu verstehen, dass das `Duration`-Objekt nicht nur für Tage verwendet werden kann. Es gibt auch andere Einheiten wie Stunden, Minuten, Sekunden, etc., die für spezifischere Berechnungen nützlich sein können.

In komplexeren Anwendungen können wir auch das `TimeZone`-Objekt nutzen, um datumsbezogene Aufgaben wie Sommer- und Winterzeit-Anpassungen zu berücksichtigen.

## Siehe auch

- [Dokumentation des `chrono`-Paketes](https://docs.rs/chrono/0.4.19/chrono/)
- [Weitere Beispiele für die Berechnung von Datumsangaben in Rust](https://gist.github.com/jaysonvirissimo/2cd6c4984f69bcb942eec2f4e93cd182)
- [Einführung in die Datums- und Uhrzeit-Programmierung in Rust](https://learning-rust.github.io/docs/a0.index/c.time.html)