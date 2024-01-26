---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:42.367781-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Zufallszahlen erzeugen heißt, nicht vorhersagbare Werte zu erstellen. Programmiert man Spiele, Tests oder Simulationen, sind sie essentiell.

## How to:
Um in Rust Zufallszahlen zu erzeugen, nutzt man das `rand`-Crate. Hier ist ein einfaches Beispiel:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let zufallszahl: u8 = rng.gen_range(0..100);
    println!("Deine Zufallszahl ist: {}", zufallszahl);
}
```

Beim Ausführen könnte die Ausgabe so aussehen:

```
Deine Zufallszahl ist: 42
```

## Deep Dive
Zufallszahlen in Programmen sind meist pseudozufällig, generiert über Algorithmen. Historisch gesehen wurden dafür oft lineare Kongruenzgeneratoren genutzt. Rusts `rand`-Crate bietet aber moderne Algorithmen wie CSPRNGs - kryptografisch sichere Pseudozufallszahlengeneratoren.

Alternativen in Rust sind die direkte Nutzung der OS-Funktionen oder kryptografische Libraries. Die `rand`-Library abstrahiert und vereinfacht den Zugriff. Zum Implementieren speziellerer Szenarien kann man eigene RNGs erstellen, indem Traits von `rand` implementiert werden.

## See Also
- Rust `rand` Crate: https://crates.io/crates/rand
- Rust Dokumentation für `rand`: https://docs.rs/rand/latest/rand/
- Tutorial zu RNGs in Rust: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
