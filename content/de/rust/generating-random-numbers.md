---
title:                "Rust: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Generierung von Zufallszahlen beschäftigen? Nun, Zufallszahlen sind in vielen Programmieranwendungen unerlässlich, sei es für Spiele, Kryptographie oder statistische Analysen. Mit der Programmiersprache Rust können wir effizient und sicher Zufallszahlen generieren.

## So geht's

Die Generierung von Zufallszahlen in Rust kann auf verschiedene Arten erfolgen, aber in diesem Blog-Beitrag werden wir uns auf die Verwendung des `rand`-Crates konzentrieren.

Zunächst müssen wir das `rand`-Crate in unsere `Cargo.toml` Datei aufnehmen:

```Rust
[dependencies]
rand = "0.8.3"
```

Dann können wir das `rand`-Crate in unserem Code importieren:

```Rust
use rand::Rng;
```

Um eine einzelne Zufallszahl zwischen 0 und 10 zu generieren, können wir die `gen_range`-Funktion verwenden:

```Rust
let random_number = rand::thread_rng().gen_range(0..11);
```

Hier verwenden wir auch die `thread_rng`-Funktion, um einen Thread-sicheren Zufallszahlengenerator zu erstellen.

Wenn wir eine Zufallszahl von einem bestimmten Datentyp generieren wollen, können wir die `gen`-Funktion verwenden und den gewünschten Datentyp angeben:

```Rust
let random_number: u32 = rand::thread_rng().gen();
```

Weitere Informationen und Beispiele zur Generierung von Zufallszahlen in Rust findest du in der [Dokumentation des `rand`-Crates](https://docs.rs/rand).

## Tiefere Einblicke

Der `rand`-Crate bietet viele Funktionen und Methoden zur Generierung von Zufallszahlen in Rust. Es verwendet eine Mischung aus verschiedenen Algorithmen, um eine hohe Qualität und zufällige Verteilung der generierten Zahlen zu gewährleisten.

Eine wichtige Sache zu beachten ist, dass Zufallszahlen in der Programmierung nie wirklich "zufällig" sind, sondern immer auf deterministischen Algorithmen basieren. Dennoch können wir mit verschiedenen Methoden wie der Verwendung von Zeitstempeln oder Benutzereingaben eine ausreichend zufällige Ergebnisse erzeugen.

Wenn du dich tiefer in die Generierung von Zufallszahlen in Rust einlesen möchtest, empfehle ich dir [diesen Artikel von George Hotz](https://www.doc.ic.ac.uk/~gs2617/hacking-article.html), in dem er verschiedene Methoden zur Erzeugung von Pseudozufallszahlen in Rust diskutiert.

## Siehe auch

- [Die offizielle Dokumentation des `rand`-Crates](https://docs.rs/rand)
- [Artikel von George Hotz über die Generierung von Zufallszahlen in Rust](https://www.doc.ic.ac.uk/~gs2617/hacking-article.html)