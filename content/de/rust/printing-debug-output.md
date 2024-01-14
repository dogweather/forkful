---
title:    "Rust: Debug-Ausgabe drucken"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmiersprachen kann es manchmal schwierig sein, den Überblick zu behalten und Probleme zu lösen. Das Debugging von Code kann eine herausfordernde Aufgabe sein, aber zum Glück gibt es einige Tools, die uns dabei unterstützen können, den Fehler in unserem Code zu finden und zu beheben. Eine Möglichkeit, dies zu erreichen, ist das Drucken von Debug-Ausgaben.

## Wie geht das?

In Rust gibt es verschiedene Möglichkeiten, Debug-Ausgaben zu erstellen. Eine davon ist die Verwendung der `println!()`-Makro, die wir in unserem Code einbinden können, um bestimmte Variablen oder Ausdrücke auf der Konsole auszugeben. Schauen wir uns ein Beispiel an:

```Rust
fn main() {
    let name = "Max";
    let age = 25;
    
    println!("Der Name des Nutzers ist {} und sein Alter beträgt {}", name, age);
}
```

Dieser Code verwendet die `println!()`-Makro, um den Inhalt der Variablen `name` und `age` auf der Konsole auszugeben. Die Ausgabe sieht folgendermaßen aus:

```
Der Name des Nutzers ist Max und sein Alter beträgt 25
```

Wir können auch beliebige Ausdrücke innerhalb der `println!()`-Makro verwenden und diese werden dann entsprechend ausgewertet und ausgegeben. Hier ist ein weiteres Beispiel:

```Rust
fn main() {
    let num1 = 5;
    let num2 = 10;
    
    println!("Das Ergebnis von {} + {} ist {}", num1, num2, num1 + num2);
}
```

Die Ausgabe wird wie folgt aussehen:

```
Das Ergebnis von 5 + 10 ist 15
```

Ein weiteres nützliches Tool für das Drucken von Debug-Ausgaben ist die `dbg!()`-Makro, die uns den Wert einer Variablen oder eines Ausdrucks zusammen mit dem Namen der Variablen und der Datei und Zeilennummer, in der die Makro verwendet wird, ausgeben kann. Hier ist ein Beispiel:

```Rust
fn main() {
    let num = 50;
    
    dbg!(num);
}
```

Die Ausgabe wird folgendermaßen aussehen:

```
[src/main.rs:4] num = 50
```

## Tiefere Einblicke

Zusätzlich zu den oben genannten Methoden gibt es noch einige andere Möglichkeiten, Debug-Ausgaben in Rust zu erstellen, die nützlich sein können, wenn man komplexeren Code debuggen muss. Dazu gehören das Drucken von Debug-Ausgaben in Unit Tests, die Verwendung von Format-Strings in der `println!()`-Makro und die Nutzung von externen Logging-Bibliotheken wie `log` und `env_logger`.

## Siehe auch

- [Rust Dokumentation: Formatierung von Strings](https://doc.rust-lang.org/std/fmt/index.html)
- [Rust Cookbook: Debug-Ausgaben erstellen](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/logging.html#output-debug-statements)
- [offizielles Rust Forum: Empfehlungen zur Auswahl einer Logging-Bibliothek](https://users.rust-lang.org/t/logging-libraries-a-survey/22369)