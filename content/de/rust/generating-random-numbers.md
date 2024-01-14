---
title:    "Rust: Generierung von Zufallszahlen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil vieler Anwendungen und kann in verschiedenen Bereichen wie Spielen, Kryptografie und Simulationen verwendet werden. Mit Rust können wir auf einfache Weise effiziente und sichere Zufallszahlen erzeugen.

## Wie

Um Zufallszahlen in Rust zu generieren, müssen wir die Standardbibliothek "rand" importieren. Dann können wir eine Instanz des Zufallszahlengenerators initialisieren und die gewünschte Anzahl an Zufallszahlen erzeugen.

```
Rust
use rand::Rng;

fn main() {
    // Initialisierung des Zufallszahlengenerators
    let mut rng = rand::thread_rng();

    // Generierung einer Zufallszahl zwischen 1 und 10
    let random_number = rng.gen_range(1, 11);

    println!("Die generierte Zufallszahl ist {}", random_number);
}
```

Output:
```
Die generierte Zufallszahl ist 8
```

Wir können auch eine Liste mit mehreren Zufallszahlen erzeugen, indem wir eine Schleife verwenden:

```
Rust
use rand::Rng;

fn main() {
    // Initialisierung des Zufallszahlengenerators
    let mut rng = rand::thread_rng();

    // Generierung von 10 Zufallszahlen zwischen 1 und 100
    for _ in 0..10 {
        let random_number = rng.gen_range(1, 101);
        println!("Die nächste Zufallszahl ist {}", random_number);
    }
}
```

Output:
```
Die nächste Zufallszahl ist 54
Die nächste Zufallszahl ist 17
Die nächste Zufallszahl ist 91
Die nächste Zufallszahl ist 2
Die nächste Zufallszahl ist 76
Die nächste Zufallszahl ist 38
Die nächste Zufallszahl ist 59
Die nächste Zufallszahl ist 23
Die nächste Zufallszahl ist 89
Die nächste Zufallszahl ist 47
```

## Deep Dive

Die "rand" Bibliothek verwendet standardmäßig den Mersenne-Twister-Algorithmus zur Generierung von Zufallszahlen. Dieser Algorithmus ist bekannt für seine gute Qualität und Effizienz. Auch die Sicherheit von Zufallszahlen ist sehr wichtig, besonders in Bereichen wie Kryptografie. Die "rand" Bibliothek sorgt dafür, dass die erzeugten Zufallszahlen kryptografisch sicher sind, indem sie zusätzliche Maßnahmen wie eine Entropiequelle und eine Zurückweisungsmethode verwendet.

## Siehe auch

Weitere Informationen und Beispiele zur Generierung von Zufallszahlen mit Rust finden Sie unter folgenden Links:

- https://docs.rs/rand/0.8.3/rand/index.html
- https://www.rust-lang.org/learn/get-started
- https://beachape.com/blog/2018/03/10/working-with-random-in-rust/