---
title:                "Protokollierung"
date:                  2024-01-26T01:03:49.616322-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Logging ist im Grunde die Art und Weise, wie wir aufzeichnen, was in unseren Programmen passiert. Es ist wie eine kleine schwarze Box; wenn Dinge schiefgehen (und glauben Sie mir, das werden sie), sind Logs unbezahlbar, um herauszufinden, was passiert ist, Probleme zu diagnostizieren und die Leistung zu optimieren.

## Wie das geht:
In Gleam würde man typischerweise eine Logging-Bibliothek einbinden – es gibt keinen dedizierten Logging-Mechanismus direkt nach der Installation. Nehmen wir an, wir verwenden eine hypothetische `gleam_logger`-Crate. So könnten Sie diese integrieren:

```gleam
import gleam_logger

pub fn main() {
  gleam_logger.info("Die Anwendung startet!")
  let result = intense_calculation()

  case result {
    Ok(value) -> 
      gleam_logger.debug("Berechnung erfolgreich", value)
    Error(err) -> 
      gleam_logger.error("Berechnung fehlgeschlagen", err)
  }
}
```

Die erwartete Ausgabe in Ihren Logs würde ungefähr so aussehen:

```
INFO: Die Anwendung startet!
DEBUG: Berechnung erfolgreich 42
ERROR: Berechnung fehlgeschlagen Grund: Division durch Null
```

## Vertiefung
Die Kunst des Loggings gibt es seit den Anfangstagen der Programmierung. Systembetreiber bekamen tatsächlich Protokolle vom Computer - um sicherzustellen, dass alles reibungslos lief. Schnell vorwärts, und Logging ist digital geworden und zu einem Kernbestandteil der Softwareentwicklung.

Obwohl Gleam, eine relativ junge Sprache, die auf das Erlang-Ökosystem abzielt, kein eingebautes Logging-Framework hat, können Sie die ausgereiften Erlang-Logging-Funktionen oder andere von der Community bereitgestellte Bibliotheken nutzen. Jede hat unterschiedliche Funktionen und Kompromisse: Einige könnten strukturiertes Logging bieten, andere sind eher für einfache Textausgaben.

Jetzt zur Frage der Implementierung einer Logging-Funktion: Ist es einfach? Auf den ersten Blick ja. Aber wenn man tiefer gräbt, betrachtet man den Umgang mit Nebenläufigkeit, I/O-Engpässen, Log-Rotation, Formatstandardisierung (denken Sie an JSON für strukturiertes Logging), Level-Filterung und möglicherweise verteiltes Tracing. Außerdem möchte man in einem funktionalen Paradigma in der Regel Nebenwirkungen (wie Logging) auf vorhersehbare und kontrollierte Weise handhaben.

## Siehe auch
Hier finden Sie mehr über die Einzelheiten des Loggings in Gleam und seinem umgebenden Ökosystem:
- [Erlangs :logger Dokumentation](http://erlang.org/doc/apps/kernel/logger_chapter.html): Da Gleam zu Erlang kompiliert, ist dies direkt anwendbar.
- [Gleams Standardbibliotheksdokumentation](https://hexdocs.pm/gleam_stdlib/): Für Updates zu Logging-Utilities, die hinzugefügt werden könnten.
- [Awesome Gleam](https://github.com/gleam-lang/awesome-gleam): Eine kuratierte Liste von Ressourcen, die auch Logging-Bibliotheken enthalten könnte, sobald diese verfügbar sind.
