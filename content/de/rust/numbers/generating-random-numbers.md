---
date: 2024-01-27 20:35:04.953061-07:00
description: "Wie geht das: Rust st\xFCtzt sich auf externe Crates f\xFCr die Generierung\
  \ von Zufallszahlen, wobei `rand` am h\xE4ufigsten verwendet wird. Um mit der Generierung\u2026"
lastmod: '2024-03-13T22:44:53.668614-06:00'
model: gpt-4-0125-preview
summary: "Rust st\xFCtzt sich auf externe Crates f\xFCr die Generierung von Zufallszahlen,\
  \ wobei `rand` am h\xE4ufigsten verwendet wird."
title: Generierung von Zufallszahlen
weight: 12
---

## Wie geht das:
Rust stützt sich auf externe Crates für die Generierung von Zufallszahlen, wobei `rand` am häufigsten verwendet wird. Um mit der Generierung von Zufallszahlen zu beginnen, müssen Sie zuerst `rand` zu Ihrer `Cargo.toml`-Datei hinzufügen:

```toml
[dependencies]
rand = "0.8.5"
```

Als Nächstes können Sie in Ihrem Rust-Code Zufallszahlen mit `rand` generieren. Hier ist ein Beispiel für die Generierung einer zufälligen Ganzzahl und einer Gleitkommazahl:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Generiert eine zufällige Ganzzahl zwischen 1 und 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Zufällige Ganzzahl: {}", random_int);
    
    // Generiert eine zufällige Gleitkommazahl zwischen 0.0 und 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Zufällige Gleitkommazahl: {}", random_float);
}
```

Ein möglicher Ausgabewert könnte sein:

```plaintext
Zufällige Ganzzahl: 7
Zufällige Gleitkommazahl: 0.9401077112175732
```

Beachten Sie, dass das erneute Ausführen des Programms unterschiedliche Werte produzieren wird.

## Vertiefung
Die Generierung von Zufallszahlen in Rust wird durch `rand` und dessen Abhängigkeiten wie `getrandom` erleichtert und stellt eine breite Abstraktion über Betriebssystemfunktionen und algorithmische Generatoren dar. Historisch gesehen hat sich die Zufälligkeit in der Informatik von einfachen, vorhersehbaren Algorithmen zu komplexen, kryptografisch sicheren Methoden entwickelt. Rusts Ansatz umfasst diese Entwicklung durch sein austauschbares `Rng`-Trait, das von verschiedenen Generatoren unterstützt werden kann, je nach der erforderlichen Zufälligkeitsqualität und Leistung.

Für die meisten Anwendungen bietet die Abhängigkeit von `rand` und dem RNG des Systems eine gute Balance zwischen Einfachheit und Entropie. Jedoch, für kryptografische Anwendungen, verweisen Crates wie `rand` auf `getrandom` für das Seeding, das sich wiederum auf betriebssystemspezifische Mechanismen stützt (z.B. `/dev/urandom` auf Unix-ähnlichen Systemen), um kryptografisch sichere Zufälligkeit zu gewährleisten.

Alternativ, wenn Sie spezifische Bedürfnisse haben, die durch `rand` nicht abgedeckt werden, könnte das Erforschen anderer Crates oder das Implementieren von benutzerdefinierten Generatoren basierend auf mathematischen Modellen ein Weg sein. Trotzdem bieten `rand` und sein Ökosystem für die überwiegende Mehrheit der Anwendungsfälle robuste Lösungen, die sowohl effizient als auch einfach in Rust-Applikationen zu integrieren sind.
