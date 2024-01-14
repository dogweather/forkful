---
title:    "Rust: Debug-Ausgabe drucken"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Drucken von Debug-Ausgaben ist eine nützliche Technik, um Probleme in Ihrem Rust-Code zu erkennen und zu beheben. Durch das Ausgeben von Informationen können Sie den genauen Ablauf Ihres Programms verfolgen und Fehler schneller finden. In dieser Anleitung erfahren Sie, wie Sie Debug-Ausgaben in Rust erstellen können.

## Wie es geht

Um Debug-Ausgaben in Rust zu erstellen, verwenden Sie die `println!` -Makrofunktion. Dieses Makro nimmt ein Formatierungszeichenfolge als Eingabe und ermöglicht es Ihnen, Variablenwerte innerhalb der Zeichenfolge anzuzeigen.

```Rust
fn main() {
    let name = "Max";
    let age = 25;

    println!("Mein Name ist {} und ich bin {} Jahre alt.", name, age);
}
```

Die Ausgabe dieses Programms wäre: `Mein Name ist Max und ich bin 25 Jahre alt.`

## Tiefer Einblick

Die `println!` -Funktion verwendet das Konzept der "String Interpolation", um Variablenwerte in die Ausgabe einzufügen. Es ersetzt Platzhalter wie `{}` in der Formatierungszeichenfolge mit den entsprechenden Werten der Variablen. Sie können auch spezifizieren, welcher Variablentyp ausgegeben werden soll, indem Sie ein `:` und den entsprechenden Datentyp nach dem Platzhalter angeben.

```Rust
fn main() {
    let pi = 3.141592653;
    let precision = 3;

    println!("Eine Näherung von Pi mit {precision} Nachkommastellen beträgt: {pi:.precision$}", precision=precision, pi=pi);
}
```

Die Ausgabe wäre:`Eine Näherung von Pi mit 3 Nachkommastellen beträgt: 3.142`

Weitere nützliche Makros für Debug-Ausgaben sind `eprintln!` (Ausgabe auf Standardfehler), `dbg!` (Ausgabe einer Variablen zusammen mit ihrem Wert) und `dbg!` (Ausgabe eines Hinterlegungsbereichs).

## Siehe auch

- [The Rust Programming Language - Debugging](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [Rust By Example - Printing](https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html)
- [Official Rust Documentation - Formatting Macros](https://doc.rust-lang.org/std/fmt/#formatting-macros)