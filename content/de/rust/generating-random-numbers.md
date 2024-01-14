---
title:    "Rust: Zufallszahlen generieren"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Generierung von zufälligen Zahlen beschäftigen? Nun, für viele Anwendungsfälle ist es wichtig, dass ein Computerprogramm zufällige Werte erzeugen kann. Dies kann beispielsweise für Computerspiele, Simulationen oder Verschlüsselungsalgorithmen nützlich sein. In diesem Blogbeitrag werden wir uns mit der Erzeugung von Zufallszahlen in der Programmiersprache Rust beschäftigen.

## Wie geht's

Um zufällige Zahlen in Rust zu generieren, verwenden wir die `rand`-Bibliothek. Zuerst müssen wir diese Bibliothek unserem Projekt hinzufügen, indem wir `rand` in unsere `Cargo.toml`-Datei aufnehmen. Dann können wir in unserem Code die Funktionen der Bibliothek aufrufen, um zufällige Zahlen zu erzeugen. Hier ist ein einfaches Beispiel, das eine zufällige Ganzzahl zwischen 1 und 10 generiert:

```rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let random_number = rng.gen_range(1, 11);
    println!("Zufällige Zahl: {}", random_number);
}
```

Die Ausgabe könnte beispielsweise `Zufällige Zahl: 7` sein. Beachten Sie, dass wir die Funktion `gen_range` aus der `Rng`-Struktur in `rand` verwenden und dass wir die Bibliothek über `use` importieren müssen.

Wir können auch eine zufällige Fließkommazahl erzeugen, indem wir `gen_range` mit Gleitkommawerten aufrufen:

```rust
let random_float: f64 = rng.gen_range(0.0, 1.0);
```

Es gibt auch andere Methoden, um zufällige Werte zu generieren, je nach unseren spezifischen Anforderungen. Weitere Informationen finden Sie in der Dokumentation der `rand`-Bibliothek.

## Tiefer eintauchen

Die `rand`-Bibliothek basiert auf dem Mersenne-Twister-Algorithmus, der eine der bekanntesten Methoden zur Erzeugung von zufälligen Zahlen ist. Dieser Algorithmus verwendet einen sogenannten Seed, der als Startwert für die Generierung von Zufallszahlen verwendet wird. Wenn wir denselben Seed in einem Programm verwenden, erhalten wir immer dieselben zufälligen Zahlen. Dies ist nützlich für Testzwecke, aber in den meisten Fällen möchten wir einen zufälligen Seed verwenden, damit wir unterschiedliche Zahlen erhalten.

Es ist auch wichtig zu beachten, dass die in der `rand`-Bibliothek generierten Zufallszahlen nicht wirklich zufällig sind, sondern durch einen Algorithmus erzeugt werden. Sie können also theoretisch vorhersagen, welche Zahlen als nächstes generiert werden. Aus diesem Grund ist es wichtig, keine kryptografisch sicheren Anwendungen auf Basis von `rand` zu entwickeln. Für solche Anwendungen gibt es spezielle Bibliotheken, die auf kryptografisch sicheren Algorithmen basieren.

## Siehe auch

- [Die `rand`-Dokumentation](https://docs.rs/rand/0.8.4/rand/)
- [Rust-Programmiersprache](https://www.rust-lang.org)
- [Mersenne-Twister-Algorithmus auf Wikipedia](https://de.wikipedia.org/wiki/Mersenne-Twister-Algorithmus)