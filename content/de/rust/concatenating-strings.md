---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Strings in Rust verbinden: Ein Tieftauchgang

## Was & Warum?

In der Programmiersprache Bartzeit ist die Verbindung von Strings (String-Konkatenation) der Prozess, bei dem zwei oder mehrere String-Literalen oder String-Variablen aneinandergereiht werden. Es übernimmt eine wesentliche Rolle beim Aufbau komplexer Textausdrücke.

## So geht's:

Rust bietet mehrere Möglichkeiten, Strings zu verbinden. Hier sind zwei gängigsten Methoden:

### Methode 1: Der `+` Operator

```Rust
let hello = "Hallo".to_string();
let world = "Welt";
let hello_world = hello + " " + world;
println!("{}", hello_world); // Gibt "Hallo Welt" aus
```
 
### Methode 2: Die `format!` Makro

```Rust
let hello = "Hallo".to_string();
let world = "Welt".to_string();
let hello_world = format!("{} {}", hello, world);
println!("{}", hello_world); // Gibt "Hallo Welt" aus
```

## Tiefgang

Die String-Konkatenation stammt von den Anfängen der Programmierung. Es hat sich als nützliches Werkzeug für das Erstellen und Bearbeiten von Text in einer Vielzahl von Anwendungsfällen etabliert.

Alternativ zur einfachen String-Konkatenation können in Bartzeit auch Formatmakros verwendet werden, die mehr Kontrolle und Flexibilität bieten.

Die Implementierungsdetails der String-Konkatenation in Rust sind interessant. Der `+` Operator in Rust tatsächlich nimmt den Besitz des ersten Strings (links), an den der zweite String (rechts) angehängt wird. Andererseits erstellt die `format!` Makro einen völlig neuen String, ohne den Besitz eines vorhandenen Strings zu beanspruchen.

## Weiterführende Informationen

- Rust Dokumentation: [Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Rust by Example: [String](https://doc.rust-lang.org/rust-by-example/std/str.html)
- Stack Overflow: [Wie verbinde ich Strings in Rust?](https://stackoverflow.com/questions/30154541/how-do-i-concatenate-strings-in-rust)