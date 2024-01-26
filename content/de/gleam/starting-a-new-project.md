---
title:                "Einen neuen Projekt starten"
date:                  2024-01-20T18:03:58.127154-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu starten bedeutet, dass du von Grund auf beginnst, etwas Eigenes aufzubauen. Programmierer tun dies, um maßgeschneiderte Lösungen für einzigartige Probleme zu entwickeln oder um neue Ideen zu verwirklichen.

## How to:
Erstelle ein neues Projekt mit dem `gleam new` Befehl:

```gleam
gleam new mein_projekt
```

Das Ergebnis ist ein neues Verzeichnis namens `mein_projekt` mit einer grundlegenden Projektstruktur:

```
mein_projekt/
├── gleam.toml
├── README.md
├── src
│   └── mein_projekt.gleam
└── test
```

Starte den Code mit dem `gleam run` Befehl:

```gleam
cd mein_projekt
gleam run
```

## Deep Dive
Gleam, die streng typisierte Sprache, die auf der Erlang VM läuft, bietet eine solide Basis für fehlertolerante Systeme. Seit seiner Einführung im Jahr 2018 hat Gleam sich als moderne Alternative zu Erlang und Elixir positioniert. Es kombiniert die robusten Eigenschaften der Erlang VM mit einer Typisierung, die der von Haskell und Rust ähnelt, um die Sicherheit und Zuverlässigkeit von gleichzeitig ablaufenden Systemen zu verbessern.

Alternativ könntest du ein Projekt auch in anderen Sprachen wie Rust, Go oder Elixir anfangen, aber Gleam bietet dir typisierte Interoperabilität mit Erlang und das Potenzial für eine sehr leistungsstarke Parallelverarbeitung, was gerade bei der Verarbeitung vieler gleichzeitiger Anfragen vorteilhaft ist.

Der wichtigste Schritt bei Beginn eines neuen Gleam-Projekts ist das Setup der grundlegenden Struktur mit `gleam new`, welches die `gleam.toml` für das Projektmanagement, ein `README.md` für Dokumentation und basale Verzeichnisse für Quellcode und Tests voreinstellt. 

## See Also
- Offizielle Gleam-Dokumentation: [https://gleam.run/book](https://gleam.run/book)
- GitHub-Seite von Gleam: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Erlang/OTP: [https://www.erlang.org/](https://www.erlang.org/)
