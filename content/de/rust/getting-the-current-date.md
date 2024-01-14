---
title:    "Rust: Das aktuelle Datum erhalten"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums kann eine häufige Aufgabe in der Programmierung sein, besonders wenn man Programme schreibt, die abhängig von der aktuellen Zeit oder Datum funktionieren. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies in Rust erreichen kann.

# Wie geht man vor

Zuerst müssen wir das `chrono`-Paket in unsere Projektabhängigkeiten aufnehmen. Dies können wir tun, indem wir die folgende Zeile in unsere `Cargo.toml`-Datei hinzufügen:

```Rust
[dependencies]
chrono = "0.4.19"
```

Als nächstes importieren wir das `chrono`-Paket in unserem Code mit der folgenden Zeile:

```Rust
// main.rs
use chrono::prelude::*;
```

Jetzt können wir das aktuelle Datum mit der `Utc::now()`-Funktion abrufen. Diese Funktion gibt ein `DateTime<UTC>`-Objekt zurück, welches das aktuelle Datum und die aktuelle Uhrzeit in der UTC-Zeitzone repräsentiert. Wir können dies in einer Variable speichern und auf verschiedene Arten anzeigen, wie im folgenden Beispiel:

```Rust
// main.rs
let current_datetime = Utc::now();
println!("Das aktuelle Datum und die aktuelle Uhrzeit in UTC: {}", current_datetime);
```

Die Ausgabe sieht folgendermaßen aus:

```
Das aktuelle Datum und die aktuelle Uhrzeit in UTC: 2021-11-03 12:31:00 UTC
```

Wir können auch das Datum auf bestimmte Teile konvertieren, z.B. in ein `Date`-Objekt, welches nur das Datum ohne Zeit enthält:

```Rust
// main.rs
let current_date = Utc::now().date();
println!("Das aktuelle Datum: {}", current_date);
```

Und die Ausgabe:

```
Das aktuelle Datum: 2021-11-03
```

Es gibt viele andere nützliche Funktionen und Methoden, die wir auf das `DateTime`-Objekt anwenden können, wie z.B. die Umrechnung in eine andere Zeitzone oder das Hinzufügen und Subtrahieren von Zeiten. Für weitere Details empfehlen wir einen Blick in die offizielle Dokumentation des `chrono`-Pakets.

# Tiefere Einblicke

Das `chrono`-Paket basiert auf dem Konzept der "zeitlichen Kalender" und bietet Unterstützung für verschiedene Kalender, Zeitzonen und Datenformate. Es ist auch sehr performant und bietet eine umfangreiche Auswahl an Datentypen und Funktionen für die Arbeit mit Zeit und Datum.

Eine interessante Funktion, die das Paket bietet, ist die Möglichkeit, die Laufzeit von Aufgaben in Code zu verfolgen. Dies kann besonders nützlich sein, wenn man Code in einem Produktionsumfeld debuggt und herausfinden möchte, welche Teile des Codes am längsten brauchen.

# Siehe auch

- [Offizielle Dokumentation des `chrono`-Pakets](https://docs.rs/chrono/0.4.19/chrono/)
- [Beispiele für die Verwendung von `chrono`](https://github.com/chronotope/chrono/tree/master/examples)
- [Einige Tipps und Tricks für die Arbeit mit Zeit und Datum in Rust](https://blog.maximebouges.com/posts/chrono_duration/)
- [Das `humantime`-Paket, welches eine menschenlesbare Darstellung von Zeit und Datum ermöglicht](https://docs.rs/humantime/2.1.0/humantime/)