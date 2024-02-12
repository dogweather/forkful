---
title:                "Nutzung einer interaktiven Shell (REPL)"
aliases:
- de/rust/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:17:46.671830-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nutzung einer interaktiven Shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Was & Warum?
Eine interaktive Shell für Rust, oder REPL (Read-Eval-Print Loop), ermöglicht es Ihnen, Rust-Code sofort auszuführen und sofortige Ergebnisse zu sehen, ideal für Experimente oder zum Lernen. Programmierer nutzen es, um Code-Schnipsel zu testen, zu debuggen oder einfach mit Sprachfunktionen zu spielen, ohne die Last, ein vollständiges Projekt zu kompilieren.

## Wie:
Bis jetzt hat Rust keine offizielle REPL, die damit ausgeliefert wird. Sie können jedoch Drittanbieter-Tools wie `evcxr_repl` verwenden. Installieren Sie es mit Cargo:

```sh
cargo install evcxr_repl
```

Dann führen Sie die REPL aus:

```sh
evcxr
```

Innerhalb testen Sie etwas Rust-Code:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Die Ausgabe sollte sein:

```
5 + 3 = 8
```

## Tiefer Eintauchen
Das Ethos von Rust konzentriert sich auf Sicherheit und Leistung, was normalerweise mit im Voraus kompilierten Sprachen assoziiert wird und weniger mit interpretierten, REPL-freundlichen. Historisch gesehen priorisierten Sprachen wie Python oder Ruby das Vorhandensein einer REPL für sofortiges Feedback, wurden jedoch nicht mit systemnahen Aufgaben im Sinn entworfen.

Trotz der Abwesenheit einer offiziellen REPL in Rust sind ein paar Alternativen wie `evcxr_repl` entstanden. Diese Projekte hacken Rust nicht einfach in eine REPL; sie verweben geschickt den Kompilier-und-Ausführ-Zyklus der Sprache in eine interaktive Sitzung. Die REPL kompiliert den Code im Hintergrund und führt die Binärdatei aus, wobei die Ausgabe erfasst wird. Auf diese Weise bewahrt sie die Leistungsvorteile von Rust, während sie dennoch diese interaktive Erfahrung bietet.

Es gibt eine laufende Diskussion in der Rust-Gemeinschaft über die offizielle Unterstützung einer REPL, und mit jeder Sprachiteration sehen wir eine zunehmende Verfeinerung der Werkzeuge, die schließlich zu einer nativen Lösung führen könnte.

## Siehe Auch
Für weitere Informationen und andere Werkzeuge:
- Evcxr REPL GitHub-Repo: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, eine Online-Möglichkeit, Rust-Code zu testen: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Diskussion über das REPL-Feature in der Rust-Sprache: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
