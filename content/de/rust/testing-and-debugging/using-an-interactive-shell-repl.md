---
date: 2024-01-26 04:17:46.671830-07:00
description: "Wie: Bis jetzt hat Rust keine offizielle REPL, die damit ausgeliefert\
  \ wird. Sie k\xF6nnen jedoch Drittanbieter-Tools wie `evcxr_repl` verwenden. Installieren\u2026"
lastmod: '2024-03-13T22:44:53.674431-06:00'
model: gpt-4-0125-preview
summary: Bis jetzt hat Rust keine offizielle REPL, die damit ausgeliefert wird.
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

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
