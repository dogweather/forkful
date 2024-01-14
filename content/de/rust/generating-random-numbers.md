---
title:                "Rust: Zufallszahlen erzeugen"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung gibt es viele Szenarien, in denen das Generieren von Zufallszahlen von entscheidender Bedeutung ist. Zum Beispiel werden Zufallszahlen häufig in Simulationen, Kryptographie und bei der Erstellung von Spielen verwendet. Rust bietet eine effiziente und robuste Möglichkeit, Zufallszahlen zu generieren, was es zu einer beliebten Wahl bei Entwicklern macht.

# Wie geht man vor?

Um in Rust Zufallszahlen zu generieren, gibt es mehrere Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der Standardbibliothek von Rust, die das Modul `rand` beinhaltet. Dieses Modul bietet verschiedene Funktionen und Strukturen, die beim Generieren von Zufallszahlen helfen.

Ein grundlegender Ansatz ist die Verwendung der Funktion `random()`, die eine Zufallszahl vom Typ `f64` zurückgibt. Hier ist ein Beispielcode, der diese Funktion verwendet:

```Rust
use rand::{thread_rng, Rng};

fn main() {
    // Zufallszahl zwischen 0 und 1 generieren
    let random_number: f64 = rand::thread_rng().gen();
    
    println!("Zufallszahl: {}", random_number);
}
```

Dieser Code generiert eine Zufallszahl zwischen 0 und 1 und gibt sie auf der Konsole aus. Sie können auch Zufallszahlen in einem bestimmten Bereich generieren, indem Sie die Methode `gen_range()` verwenden. Hier ist ein Beispiel, das eine Zufallszahl zwischen 1 und 100 generiert:

```Rust
// Zufallszahl zwischen 1 und 100 generieren
let random_number: u32 = rand::thread_rng().gen_range(1, 101);

println!("Zufallszahl: {}", random_number);
```

Es gibt auch andere nützliche Funktionen und Strukturen im Modul `rand`, wie z.B. die Möglichkeit, eine Reihe von Zufallszahlen mit der Funktion `gen_iter()` zu generieren oder die Verwendung von benutzerdefinierten Datentypen für Zufallszahlen mit der Struktur `ThreadRng`. Fühlen Sie sich frei, die offizielle Dokumentation von Rust für weitere Informationen zu durchsuchen.

# Tiefere Einblicke

Das Generieren von Zufallszahlen ist keine triviale Aufgabe und erfordert ein grundlegendes Verständnis von Konzepten wie Pseudozufälligkeit und Seed. Rust verwendet standardmäßig einen Zufallsgenerator auf Basis von Xorshift, der eine gute Balance zwischen Geschwindigkeit und Zufälligkeit bietet.

Es ist auch wichtig zu beachten, dass bei Verwendung von Zufallszahlen in einer Schleife, wie z.B. in einem Spiel, der gleiche Seed verwendet wird und somit die gleichen Zufallszahlen generiert werden. In solchen Fällen ist es wichtig, einen anderen Seed zu verwenden, um eine bessere Mischung der Zufallszahlen zu erzielen.

# Siehe auch

- Dokumentation: https://doc.rust-lang.org/std/rand/
- Offizielle Rust-Website: https://www.rust-lang.org/
- Weitere Informationen über Zufallszahlen und deren Verwendung: https://de.wikipedia.org/wiki/Zufallszahlen