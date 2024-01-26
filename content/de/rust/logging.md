---
title:                "Protokollierung"
date:                  2024-01-26T01:08:00.287664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"
programming_language: "Rust"
category:             "Rust"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?

Logging ist wie das Führen eines Tagebuchs für Ihre Anwendung; es ist die Praxis, Ereignisse, Fehler und andere relevante Daten während der Laufzeit aufzuzeichnen. Entwickler nutzen Logs, um Probleme zu diagnostizieren, das Systemverhalten zu überwachen und Erkenntnisse zu sammeln, die Verbesserungen vorantreiben – es ist das A und O der Betriebsintelligenz.

## Wie geht das:

Lassen Sie uns ein grundlegendes Logging-Szenario in Rust mit Hilfe der `log`-Crate einrichten, die eine Logging-Fassade bietet, und `env_logger`, einer Logging-Implementierung für die `log`-Crate. Zuerst fügen Sie diese Ihrem Cargo.toml hinzu:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Jetzt richten Sie den Logger in Ihrer `main.rs` ein und initialisieren ihn:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Das ist eine Info-Nachricht.");
    warn!("Das ist eine Warnmeldung.");
}
```

Führen Sie Ihre App mit `RUST_LOG=info cargo run` aus, und Sie werden die Ausgabe sehen:

```
INFO: Das ist eine Info-Nachricht.
WARN: Das ist eine Warnmeldung.
```

Experimentieren Sie mit der Umgebungsvariablen `RUST_LOG`, indem Sie sie auf `error`, `warn`, `info`, `debug` oder `trace` setzen, um die Ausführlichkeit Ihrer Logs zu steuern.

## Tiefergehende Betrachtung

Das Konzept des Loggings ist nichts Neues; es gibt es seit den frühen Tagen der Computertechnik. Bevor Logging in der Software üblich wurde, verließen sich Entwickler auf primitive Methoden wie Druckanweisungen oder Debugger-Tools, um die Programmausführung nachzuvollziehen. Als Programme komplexer wurden, stieg auch der Bedarf an strukturierten Ansätzen zum Logging.

In Rust abstrahiert die `log`-Crate die Implementierungsdetails des Loggings, sodass Entwickler verschiedene Logging-Backends einstecken können. Während `env_logger` eine häufige Wahl ist, gibt es Alternativen wie `fern`, `slog` oder `tracing`, die jeweils ihre eigenen Funktionen und Konfigurationsoptionen haben.

Einige Überlegungen bei der Implementierung von Logging umfassen:

1. **Log-Level**: Die Kontrolle der Ausführlichkeit ist entscheidend. Die `log`-Crate von Rust definiert mehrere Log-Level: error, warn, info, debug und trace in absteigender Reihenfolge der Schwere.

2. **Leistung**: Logging kann die Leistung beeinträchtigen. Es ist entscheidend, es wohlüberlegt zu verwenden, insbesondere um Logging in leistungsrelevanten Pfaden zu vermeiden oder übertrieben ausführliche Logs in der Produktion.

3. **Strukturiertes Logging**: Moderne Best Practices beinhalten strukturiertes Logging, bei dem Logs in einem maschinenlesbaren Format wie JSON geschrieben werden. Bibliotheken wie `slog` ermöglichen strukturiertes Logging in Rust, das mit Log-Management-Systemen wie dem ELK Stack oder Splunk indiziert und abgefragt werden kann.

4. **Asynchrones Logging**: Um den Einfluss auf die Hauptanwendung zu minimieren, kann Logging asynchron durchgeführt werden. Dies wird oft erreicht, indem die Logging-Bibliothek in eine In-Memory-Warteschlange schreibt und ein separater Thread die Warteschlange verarbeitet und die Logs an das Ziel schreibt.

5. **Konfiguration**: Viele Logging-Frameworks unterstützen die Konfiguration über Umgebungsvariablen, Konfigurationsdateien und/oder Code. Diese Flexibilität ist entscheidend für die Feinabstimmung der Ausgabe in verschiedenen Umgebungen (Entwicklung, Staging, Produktion).

## Siehe auch

- Die Dokumentation der `log`-Crate: https://docs.rs/log/
- Die Dokumentation der `env_logger`-Crate: https://docs.rs/env_logger/
- Rust by Example Logging-Seite: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- Die `slog`-Crate, ein alternatives Logging-Framework: https://github.com/slog-rs/slog
- Tracing, ein Framework zum Instrumentieren von Rust-Programmen: https://crates.io/crates/tracing