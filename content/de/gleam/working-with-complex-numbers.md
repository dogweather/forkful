---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:40:39.371190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen bestehen aus einem Realteil und einem Imaginärteil (`a + bi`). Sie sind nützlich in verschiedenen Bereichen wie der Elektrotechnik und der Quanteninformatik. Programmierer verwenden sie, um Gleichungen zu modellieren, die nicht nur mit reellen Zahlen lösbar sind.

## Wie geht das:
Gleam hat keine native Unterstützung für komplexe Zahlen. Normalerweise müsste man seine eigene Implementierung schreiben oder eine Bibliothek finden. Hier ist ein schnelles Beispiel, wie man grundlegende Operationen implementieren könnte:

```gleam
type Complex {
  Complex(Float, Float)
}

fn add(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a + x, b + y)
}

fn multiply(c1: Complex, c2: Complex) -> Complex {
  let Complex(a, b) = c1
  let Complex(x, y) = c2
  Complex(a*x - b*y, a*y + b*x)
}

fn main() {
  let num1 = Complex(1.0, 2.0)
  let num2 = Complex(3.0, 4.0)
  let sum = add(num1, num2)
  let product = multiply(num1, num2)

  sum // Complex(4.0, 6.0)
  product // Complex(-5.0, 10.0)
}
```

## Vertiefung

Komplexe Zahlen wurden erstmals formeller von Gerolamo Cardano im 16. Jahrhundert dokumentiert. Sie sind eine natürliche Erweiterung der reellen Zahlen. Allerdings sind in einer jungen Sprache wie Gleam – die Leistung und Typsicherheit priorisiert – solche Funktionen nur rudimentär vorhanden (oder man muss sie selbst implementieren).

In einigen anderen Sprachen, wie Python, sind komplexe Zahlen eingebaut (`3+4j`), was das Leben einfacher macht. In Rust oder Haskell gibt es Bibliotheken, die fortgeschrittene Funktionalitäten direkt aus der Box bieten.

Gleams Ansatz bedeutet, dass man sich um alle Aspekte kümmern muss: Arithmetik, Polarkoordinaten, exponentielle Formen usw. Die Implementierung effizienter, genauer Operationen erfordert sorgfältige Programmierung, unter Berücksichtigung, wie das Verhalten von Fließkommazahlen Ihre Ergebnisse beeinflussen kann.

Denken Sie daran, gründlich zu testen, insbesondere Randfälle! Die Behandlung von komplexer Unendlichkeit und NaN (nicht eine Zahl) Werten kann problematisch sein, wenn man nicht vorsichtig ist.

## Siehe auch
Für weitere Informationen, hier sind einige Ressourcen, in die Sie eintauchen können:
- [Gleams offizielle Dokumentation](https://gleam.run/documentation/)
- Inspiration finden Sie in den Bibliotheken anderer Sprachen, wie Rusts [num-complex](https://crates.io/crates/num-complex) oder Pythons [cmath Modul](https://docs.python.org/3/library/cmath.html).