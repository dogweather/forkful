---
title:                "Zufallszahlen generieren"
html_title:           "Rust: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Sich mit Zufallszahlen zu beschäftigen kann in vielen Situationen nützlich sein. Zum Beispiel in der Spieleentwicklung, bei der Simulation von Daten oder beim Testen von Software.

## Wie geht's?

Um in Rust Zufallszahlen zu generieren, können wir das `rand` Crate verwenden. Zunächst müssen wir es in unserem `Cargo.toml` File hinzufügen:

```Rust
[dependencies]
rand = "0.8.4"
```

Als nächstes importieren wir das Crate in unserem Code:

```Rust
use rand::Rng;
```

Jetzt können wir verschiedene Funktionen aus dem Crate nutzen, um Zufallszahlen zu generieren. Zum Beispiel können wir mit `thread_rng()` einen Thread-sicheren Zufallsgenerator erstellen und mit `gen_range()` eine Zufallszahl aus einem bestimmten Bereich generieren:

```Rust
let my_number = rand::thread_rng().gen_range(1, 10);
// my_number kann jetzt eine Zahl zwischen 1 und 10 sein
```

Um Zufallszahlen mit einem bestimmten Datentyp zu generieren, können wir die `gen()` Funktion nutzen, die einen Wert vom Typ `u32` zurückgibt:

```Rust
let my_number: u32 = rand::thread_rng().gen();
```

Man kann auch Zufallszahlen mit einer bestimmten Verteilung erzeugen, wie zum Beispiel gleichmäßig oder normal verteilt. Dafür gibt es verschiedene Funktionen wie `gen_range` oder `gen_normal` in dem Crate.

## Tieferes Eintauchen

Das `rand` Crate bietet noch viele weitere Funktionen und Optionen für die Generierung von Zufallszahlen. Man kann zum Beispiel auch eigene Zufallszahlengeneratoren erstellen oder spezielle Generatoren für benutzerdefinierte Datentypen nutzen.

Eine wichtige Sache, die man beim Generieren von Zufallszahlen bedenken sollte, ist, dass diese nicht wirklich zufällig sind. Sie werden mithilfe von mathematischen Formeln und sogenannten "Seed"-Werten erzeugt. Wenn man also den gleichen Seed benutzt, erhält man auch die gleichen Zufallszahlen. Das kann beim Testen von Software hilfreich sein, sollte aber bedacht werden, wenn es um Sicherheit oder Verschlüsselung geht.

## Siehe auch

- [offizielle Dokumentation des `rand` Crates](https://docs.rs/rand/0.8.4/rand/)
- [Rust Book: Zufallszahlen](https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html)