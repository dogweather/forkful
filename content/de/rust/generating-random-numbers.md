---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen bezieht sich auf die Erstellung von Zahlen, die nicht vorhersehbar sind. Es ist sehr nützlich in der Spieleprogrammierung, Verschlüsselung und sogar bei Simulationen.

## So geht's: 

Hier ist ein einfaches Beispiel, wie man in Rust eine Zufallszahl erstellt:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let zufallszahl: u8 = rng.gen();
    println!("Ihre zufällige Zahl ist: {}", zufallszahl);
}
```

Wenn Sie dieses Programm ausführen, wird eine Zahl zwischen 0 und 255 angezeigt.

## Tiefgang:

Historisch gesehen wurden Zufallszahlen in der zuversichtlichen Wissenschaft verwendet, wo Ergebnisse reproduzierbar sein mussten. In der modernen Programmentwicklung, obwohl wir Pseudozufallszahlen und echte Zufallszahlen haben, sind erstere wegen der Berechenbarkeit gerne gesehen.

Alternativen zur Standard-zufälligen Generierung in Rost sind Abhängigkeiten von Dritt-Anbietern wie `rand::Rng`, `getrandom` usw. 

Die Implementierungsdetails hängen von der genauen Abhängigkeit und der Version von Rust ab, die Sie verwenden, aber im Allgemeinen basiert das Generieren von Zufallszahlen auf einem Algorithmus, der eine Zahlenreihe erstellt, die so aussieht, als wäre sie zufällig.

## Siehe Auch:

Weitere Informationen darüber, wie Sie Zufallszahlen in Rust erstellen können, finden Sie in der [Rand-Dokumentation](https://docs.rs/rand/0.8.4/rand/index.html). Für eine Tiefgreifende Diskussion über Zufallszahlen und ihre Verwendung in der Programmierung, lesen Sie [Zufallszahlen in der Informatik](https://en.wikipedia.org/wiki/Random_number_generation).