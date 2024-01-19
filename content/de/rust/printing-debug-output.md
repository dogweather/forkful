---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Printen von Debug-Ausgaben ermöglicht das gezielte Untersuchen des Programmzustands während der Laufzeit. Programmierer tun dies, um Fehler aufzuspüren oder um das Verhalten des Codes besser zu verstehen.

## Wie es funktioniert:

Das Printen von Debug-Ausgaben in Rust ist denkbar einfach. Die wichtigsten Befehle dafür sind `println!` und `dbg!`. Hier sind sie in Aktion:

```Rust
fn main() {
    let a = 3;
    let b = 4;
    println!("Summe = {}", a + b);
    dbg!(a, b);
}
```

Dieser Code liefert die Ausgabe:

```
Summe = 7
[a.rs:5] a = 3
[b.rs:5] b = 4
```

## Hintergrundinfos:

Die `dbg!`-Makro wurde in Rust 1.32 eingeführt und ermöglicht einfaches Debuggen. Alternativ können wir auch `println!` mit dem `{:?}`-Platzhalter für Debug-Ausgaben verwenden. Der Unterschied liegt darin, dass `dbg!` zusätzliche Informationen wie den Dateinamen und die Zeilenzahl liefert.

```Rust
println!("{:?}", a);  // Ausgabe: 3
```

Es ist aber anzumerken, dass nicht alle Typen mit `{:?}` ausgegeben werden können. Sie müssen das Trait `std::fmt::Debug` implementieren. Für eingebaute Typen ist das meistens kein Problem, aber eigene Strukturen müssen das explizit bekanntgeben:

```Rust
#[derive(Debug)]
struct Punkt {
    x: i32,
    y: i32,
}
```

Nun kann man `Punkt`-Instanzen mit `println!` ausgeben.

## Weiterführende Links:

- Rust-Dokumentation zur `println!`-Makro: https://doc.rust-lang.org/std/macro.println.html
- Rust-Dokumentation zur `dbg!`-Makro: https://doc.rust-lang.org/std/macro.dbg.html
- Rust-Format-Dokumentation: https://doc.rust-lang.org/std/fmt/