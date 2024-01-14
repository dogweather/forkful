---
title:                "Rust: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum solltest du dich mit dem Hauptbuchstaben eines Strings beschäftigen? Es gibt viele Gründe, aber einer der wichtigsten ist die Konsistenz. Wenn du in deinem Programm Logik hast, die davon ausgeht, dass alle Strings großgeschrieben sind, aber tatsächlich einige klein geschrieben sind, kann das zu unerwartetem Verhalten führen. Das Festlegen der Groß- oder Kleinschreibung von Strings kann auch dazu beitragen, Sicherheitslücken zu vermeiden.

## Wie geht das?

Es gibt mehrere Möglichkeiten, einen String in Rust großzuschreiben. Hier sind einige Beispielcodes mit den entsprechenden Ausgaben innerhalb von "```Rust ... ```" Codeblöcken:

### Verwendung der `to_uppercase()` Methode

```Rust
let string = String::from("hallo welt");
let capitalized = string.to_uppercase();
println!("{}", capitalized); // Ausgabe: HALLO WELT
```

### Mit Hilfe des `String::from_uppercase()` Konstruktors

```Rust
let string = String::from("hallo welt");
let capitalized = String::from_uppercase(string);
println!("{}", capitalized); // Ausgabe: HALLO WELT
```

### Verwendung einer Schleife und `push()` Methode

```Rust
let mut string = String::from("hallo welt");
let mut capitalized = String::new();
for c in string.chars() {
    capitalized.push(c.to_uppercase().next().unwrap());
}
println!("{}", capitalized); // Ausgabe: HALLO WELT
```

Es gibt auch weitere Methoden wie `replace()` oder `chars()` und `map()` Kombinationen, die zum Großschreiben eines Strings in Rust verwendet werden können.

## Tiefere Einblicke

Wenn du dich fragst, warum in Rust keine standardmäßige Funktion zum Großschreiben von Strings vorhanden ist, liegt dies daran, dass Rust Strings auf Unicode basieren und es mehrere Möglichkeiten gibt, einen Buchstaben in Großschreibung zu setzen. Dies kann je nach Kontext und Sprache variieren. Daher überlässt Rust es den Entwicklern, die passende Methode für ihre spezifische Anwendung zu wählen.

## Siehe auch

- [Rust String Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [String Methoden in Rust](https://www.educative.io/edpresso/what-are-the-string-methods-in-rust)
- [String Manipulation in Rust](https://www.rust-lang.org/learn/introduction#string-manipulation)