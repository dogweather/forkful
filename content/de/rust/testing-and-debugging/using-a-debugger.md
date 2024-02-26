---
date: 2024-01-26 04:10:03.292124-07:00
description: "Ein Debugger zu verwenden ist, als w\xFCrde man sich selbst R\xF6ntgenblick\
  \ geben, um einen Blick in die Ausf\xFChrung seines Codes zu werfen. Programmierer\
  \ tun\u2026"
lastmod: '2024-02-25T18:49:50.745046-07:00'
model: gpt-4-0125-preview
summary: "Ein Debugger zu verwenden ist, als w\xFCrde man sich selbst R\xF6ntgenblick\
  \ geben, um einen Blick in die Ausf\xFChrung seines Codes zu werfen. Programmierer\
  \ tun\u2026"
title: Einsatz eines Debuggers
---

{{< edit_this_page >}}

## Was & Warum?

Ein Debugger zu verwenden ist, als würde man sich selbst Röntgenblick geben, um einen Blick in die Ausführung seines Codes zu werfen. Programmierer tun dies, um Fehler zu finden, den Programmablauf zu verstehen und sicherzustellen, dass ihr Code pfeifensauer ist. Es ist, als hätte man einen Freund, der genau darauf hinweist, wo man gestolpert ist.

## Wie:

Rust unterstützt verschiedene Debugger, aber ein gängiger ist `gdb` für GNU/Linux oder `lldb` für macOS. Man könnte auch `rust-gdb` oder `rust-lldb` verwenden, welche Wrapper sind, die Rust-Werte schön formatieren. Hier ist ein Einblick:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter ist bei: {}", counter);
    }
}
```

Um dies zu debuggen, kompilieren Sie mit Debug-Informationen:

```shell
$ rustc -g counter.rs
```

Dann führen Sie es in `rust-gdb` aus:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter ist bei: 1
(gdb) print counter
$2 = 1
```

## Tiefere Einblicke

Das Debuggen gibt es schon seit den *alten Zeiten* der Lochkarten, und seine Entwicklung war ein Segen. Rust bietet seine eigene Werkzeugunterstützung mit Integrationen für GDB und LLDB aufgrund der systemnahen Natur der Sprache.

Alternativen zum Debuggen von Rust-Code umfassen die Verwendung von integrierten Entwicklungsumgebungen (IDEs) mit ihren eingebauten Debuggern, die einige intuitiver finden. Beliebte umfassen CLion mit dem Rust-Plugin oder Visual Studio Code mit der Rust-Erweiterung.

Was die Implementierung betrifft, so erzeugt Rust Debugsymbole, die diese Debugger verstehen, was für das Durchschreiten des Codes, das Setzen von Haltepunkten und das Inspektionieren von Variablen ohne den Verstand zu verlieren, von entscheidender Bedeutung ist.

## Siehe auch

- Das Rust-Buch zum Thema Debugging: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust anhand von Beispielen zu Fehlern und Debugging: https://doc.rust-lang.org/rust-by-example/error.html
- Der Rust Language Server (RLS), der die Rust-Erweiterung von VS Code unterstützt: https://github.com/rust-lang/rls
- Debugging Rust mit Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
