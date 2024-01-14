---
title:                "Rust: Debug-Ausgabe drucken"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug-Ausgaben kann nützlich sein, um Fehler in Ihrer Rust-Codebasis zu finden und zu beheben. Es ermöglicht Ihnen, Informationen über den Zustand Ihrer Variablen und Datenstrukturen während der Ausführung Ihres Programms zu erhalten, was insbesondere bei komplexeren Anwendungen sehr hilfreich sein kann.

# Wie

Um Debug-Ausgaben in Rust zu drucken, können Sie die `println!`-Makro verwenden. In den folgenden Beispielen werden wir eine einfache Variable `name` erstellen und sie dann mithilfe der `println!`-Makro debuggen. Die Debug-Ausgabe wird auf der Konsole gedruckt, wenn Sie das Programm ausführen.

```Rust
let name = "Max Mustermann";
println!("Name: {}", name);
```

Dies wird die Ausgabe `Name: Max Mustermann` auf der Konsole drucken.

Sie können auch mehrere Variablen in einem `println!`-Aufruf debuggen, indem Sie sie durch ein Komma trennen.

```Rust
let age = 30;
let profession = "Softwareentwickler";
println!("Alter: {}, Beruf: {}", age, profession);
```

Dies wird die Ausgabe `Alter: 30, Beruf: Softwareentwickler` erzeugen.

Eine weitere nützliche Methode zum Drucken von Debug-Ausgaben ist `dbg!`, die sowohl den Wert einer Variablen als auch den Namen der Variablen druckt.

```Rust
let city = "Berlin";
dbg!(city);
```

Dies wird die Ausgabe `city = "Berlin"` erzeugen.

# Deep Dive

Es gibt verschiedene Möglichkeiten, wie Sie die `println!`- und `dbg!`-Makros verwenden können, um Debug-Ausgaben zu drucken. Eine davon ist die Verwendung von Formatierungsplatzhaltern, die es Ihnen ermöglichen, formatierte Zeichenketten und Werte auszugeben.

```Rust
let age = 30;
let profession = "Softwareentwickler";
println!("Ich bin {} Jahre alt und mein Beruf ist {}", age, profession);
```

Dies wird die Ausgabe `Ich bin 30 Jahre alt und mein Beruf ist Softwareentwickler` erzeugen.

Sie können auch verwendete Variablen mit einem Debug-Formatierungsplatzhalter drucken, indem Sie `:?` anstelle von `{}` verwenden.

```Rust
let numbers = vec![1, 2, 3];
println!("Zahlen: {:?}", numbers);
```

Dies wird die Ausgabe `Zahlen: [1, 2, 3]` erzeugen.

Es ist auch möglich, weitere Argumente an die `println!`- und `dbg!`-Makros zu übergeben, wie zum Beispiel mathematische Ausdrücke oder Funktionen.

```Rust
let x = 5;
let y = 10;
println!("x + y = {}", x + y);
```

Dies wird die Ausgabe `x + y = 15` erzeugen.

# Siehe auch

- [Die Rust Dokumentation zu Debug-Ausgaben](https://doc.rust-lang.org/std/macro.dbg.html)
- [Ein Tutorial zu Debugging in Rust](https://dev.to/lampewebdev/debugging-rust-code-with-println-dbgrumen-2ldo)
- [Der offizielle Rust Discord-Server für Hilfestellung und Diskussionen](https://discord.gg/rust-lang)