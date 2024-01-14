---
title:    "Ruby: Zufallszahlen generieren"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Generieren von zufälligen Zahlen ist ein Schlüsselelement im Ruby Programmieren. Es ermöglicht Entwicklern, sichere Passwörter, zufällige Zahlencodes oder eine Vielzahl von anderen Anwendungen zu erstellen, die zufällige Zahlen benötigen.

## Wie man es macht

Es gibt mehrere Möglichkeiten, um in Ruby zufällige Zahlen zu generieren. Eine davon ist die `rand`-Funktion, die eine zufällige Gleitkommazahl zwischen 0 und 1 zurückgibt.

```Ruby
rand # 0.8301278610310736
rand # 0.5720940406395727
rand # 0.9699427929522021
```

Man kann auch die `rand`-Funktion mit einem Argument verwenden, um eine zufällige Ganzzahl in einem bestimmten Bereich zu generieren.

```Ruby
rand(10) # 4
rand(10) # 8
rand(10) # 2
```

Um eine zufällige Zahl zwischen zwei bestimmten Werten zu erstellen, kann man die Formel `rand(max-min) + min` verwenden.

```Ruby
rand(10-5) + 5 # 8
rand(10-5) + 5 # 9
rand(10-5) + 5 # 6
```

## Tiefergehende Informationen

Die `rand`-Funktion verwendet standardmäßig den Systemzeitstempel als Seed, um pseudo-zufällige Zahlen zu generieren. Das Seed kann jedoch auch manuell gesetzt werden, indem man `srand` mit einer beliebigen Zahl als Argument aufruft.

```Ruby
srand(12345)
rand # 0.45676654043610995
rand # 0.6420109550311959
rand # 0.8807432638651273
```

Man kann auch einen Seed-Generator wie die `SecureRandom`-Klasse verwenden, um sicherere und zufälligere Zahlen zu generieren.

## Siehe auch

- [Offizielle Ruby Dokumentation zur `rand`-Funktion](https://ruby-doc.org/core-3.0.1/Kernel.html#method-i-rand)
- [Blog-Beitrag zur Verwendung von SecureRandom in Ruby](https://www.gitarts.com/blog/use-secure-random-in-ruby-to-generate-unpredictable-random-numbers/)