---
title:    "Rust: Verkettung von Zeichenfolgen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand Strings in Rust konkatenieren wollen? Eines der Hauptziele von Rust ist es, sichereren Code zu schreiben, und die Möglichkeit, Strings zu konkatenieren, ist ein wichtiger Bestandteil davon. Durch die Nutzung von Rusts starken Typprüfungen und Borrowing-System können Fehler vermieden werden, die bei anderen Programmiersprachen, wie zum Beispiel C, auftreten können.

## Wie geht's

In Rust gibt es mehrere Möglichkeiten, Strings zu verketten. Eine Möglichkeit ist die `+`-Operator-Methode, bei der Strings einfach mit `+` verbunden werden können. Zum Beispiel:

```Rust
let first_name = "John";
let last_name = "Doe";
let full_name = first_name + " " + last_name;

println!("{}", full_name); // Ausgabe: John Doe
```
Eine andere Möglichkeit ist die `format!`-Makro, bei der Platzhalter im String durch Variablen ersetzt werden. Hier ein Beispiel:

```Rust
let age = 25;
let message = format!("Ich bin {} Jahre alt.", age);

println!("{}", message); // Ausgabe: Ich bin 25 Jahre alt.
```

Es gibt auch die `push_str`-Methode, die einen String an einen bereits bestehenden anhängt. Zum Beispiel:

```Rust
let mut message = String::from("Hallo");
message.push_str(", wie geht es dir?");

println!("{}", message); // Ausgabe: Hallo, wie geht es dir?
```

## Tiefer in die Materie

In Rust werden Strings als UTF-8-codierte Byte-Sequenzen dargestellt. Beim Konkatenieren von Strings werden diese Bytes aneinander gereiht, wobei auf die Gültigkeit der UTF-8-Codierung geachtet wird. Das bedeutet, dass das Ergebnis einer Konkatenation immer ein gültiger String sein wird. Dies hilft, Fehler und potenzielle Sicherheitslücken zu vermeiden.

Es ist wichtig zu beachten, dass Strings in Rust immutable sind, dh sie können nicht verändert werden. Wenn also eine Zeichenkette konkatiniert wird, wird in Wirklichkeit ein neuer String mit dem Ergebnis der Konkatenation erstellt. Dies hat den Vorteil, dass mögliche Seiteneffekte vermieden werden und es einfacher wird, sichereren Code zu schreiben.

## Siehe auch

- [Offizielle Dokumentation zu Strings in Rust](https://doc.rust-lang.org/std/string/)
- [Rust-String-Cheat-Sheet](https://www.meganmcdevitt.com/rust-string-cheat-sheet/)