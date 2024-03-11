---
date: 2024-01-26 01:16:07.214805-07:00
description: "Code in Funktionen zu organisieren bedeutet, Ihr Programm in wiederverwendbare,\
  \ modulare Bl\xF6cke zu gliedern, die durch einen Namen identifiziert werden.\u2026"
lastmod: '2024-03-11T00:14:27.569221-06:00'
model: gpt-4-0125-preview
summary: "Code in Funktionen zu organisieren bedeutet, Ihr Programm in wiederverwendbare,\
  \ modulare Bl\xF6cke zu gliedern, die durch einen Namen identifiziert werden.\u2026"
title: Code in Funktionen organisieren
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, Ihr Programm in wiederverwendbare, modulare Blöcke zu gliedern, die durch einen Namen identifiziert werden. Wir machen das, um unseren Code sauberer, lesbarer und einfacher zu debuggen zu machen. Es geht darum, uns nicht zu wiederholen und Updates zu vereinfachen.

## Wie geht das:
Angenommen, Sie haben Code, der mehrmals die Fläche eines Kreises berechnet. Statt die Formel zu wiederholen, packen Sie sie in eine Funktion.

```Rust
fn berechne_kreisflaeche(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let flaeche = berechne_kreisflaeche(radius);
    println!("Die Fläche des Kreises beträgt: {}", flaeche);
}
```

Ausgabe:

```
Die Fläche des Kreises beträgt: 78.53981633974483
```

## Tiefergehend
Historisch gesehen stammen Funktionen aus der Mathematik, wo sie Eingaben auf Ausgaben abbilden. Beim Programmieren gibt es sie seit den Tagen der Assemblersprache, obwohl wir sie damals "Unterprogramme" nannten. Rust-Funktionen können Werte und sogar andere Funktionen zurückgeben, dank First-Class-Funktionen und Closures.

Alternativen? Inline-Code oder Makros, aber für komplexe Logik sind sie unübersichtlich. Objekte mit Methoden sind eine andere Art, Funktionalität zu organisieren, ein anderer Geschmack als eigenständige Funktionen.

Die Implementierung in Rust ist ziemlich unkompliziert. Funktionen deklarieren ihre Parametertypen und Rückgabetypen. Sie werden nach Konvention in 'snake case' benannt. Es gibt öffentliche Funktionen (`pub fn`) zur Verwendung außerhalb des Moduls und private für interne Verwendung. Und Rust hat dieses coole Feature, bei dem Sie kein `return`-Schlüsselwort für den letzten Ausdruck in einer Funktion benötigen.

## Siehe auch
Schauen Sie sich diese für mehr Informationen an:
- Das Rust-Programmiersprachenbuch: [Funktionen](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust by Example zu [Funktionen](https://doc.rust-lang.org/rust-by-example/fn.html)
