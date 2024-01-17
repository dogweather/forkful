---
title:                "Ausgabe von Debug-Meldungen"
html_title:           "Rust: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Was & Warum?

Debugging oder Fehlerbehebung ist ein wichtiger Teil des Programmierprozesses. Beim Debugging wollen Programmierer*innen herausfinden, wo ihr Code Fehler aufweist und wie sie diese beheben können. Ein nützliches Werkzeug beim Debugging ist das Drucken von Debug-Ausgaben. Dabei wird gezielt Code eingefügt, der dem Programmierer oder der Programmiererin hilft, den Ablauf des Codes nachzuvollziehen und Fehler zu finden.

# Wie?

In Rust gibt es die Makro-Funktion ```println!()```, die speziell für das Ausgeben von Debug-Ausgaben gedacht ist. Diese Akro nimmt Format-Strings und Argumente entgegen, ähnlich wie die bekannte ```printf()``` Funktion in C. Hier ist ein Beispielcode:

```Rust
fn main() {
    let name = "Max";
    let age = 25;
    println!("Name: {} | Age: {}", name, age);
}
```
Dieser Code gibt die Debug-Ausgabe `Name: Max | Age: 25` in der Konsole aus.

# Tief ins Detail

Das Drucken von Debug-Ausgaben ist in der Programmierung schon lange üblich. Vor der Einführung von Funktionen wie ```println!()``` wurde oft die Funktion ```printf()``` in C verwendet. Die Entscheidung, Debug-Ausgaben mit Makros wie ```println!()``` zu implementieren, wurde in Rust getroffen, um die Lesbarkeit und Effizienz des Codes zu verbessern.

Eine Alternative zu ```println!()``` ist die Funktion ```eprintln!()```, die Debug-Ausgaben auf den Standardfehlerstrom ausgibt. Dies kann hilfreich sein, wenn die Ausgaben nicht zusammen mit dem normalen Programmablauf ausgegeben werden sollen.

Die Implementierung von ```println!()``` ist ziemlich einfach, da das Programm einfach den gegebenen Format-String und die Argumente kombiniert und auf dem Standardausgabestrom ausgibt. Allerdings kann es bei der Fehlerbehandlung Probleme geben, da der Programmablauf unterbrochen werden kann, wenn die Ausgabe aufgrund von Fehlern fehlschlägt.

# Siehe auch

- Offizielle Dokumentation zu ```println!()```: [https://doc.rust-lang.org/std/macro.println.html](https://doc.rust-lang.org/std/macro.println.html)
- Eine Einführung in das Debugging in Rust: [https://www.youtube.com/watch?v=QQvpe5OPbH4](https://www.youtube.com/watch?v=QQvpe5OPbH4)
- Vergleich von Makros und Funktionen in Rust: [https://doc.rust-lang.org/book/ch19-06-macros.html#functions-vs-macros](https://doc.rust-lang.org/book/ch19-06-macros.html#functions-vs-macros)