---
title:                "Umgang mit komplexen Zahlen"
aliases: - /de/haskell/working-with-complex-numbers.md
date:                  2024-01-26T04:41:18.173947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Komplexe Zahlen, bestehend aus einem Real- und einem Imaginärteil, sind in verschiedenen Berechnungsbereichen wie Ingenieurwesen, Physik und Signalverarbeitung unerlässlich. Programmierer verwenden sie, um Gleichungen zu lösen, die mit reellen Zahlen nicht lösbar sind, wie zum Beispiel die Wurzeln aus negativen Zahlen zu finden.

## Wie geht das:

Haskell behandelt komplexe Zahlen mit dem Modul `Data.Complex`. Hier ist ein schneller Überblick:

```haskell
import Data.Complex

-- Definiere zwei komplexe Zahlen
let z1 = 3 :+ 4  -- das ist 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- Arithmetische Operationen
let summe = z1 + z2  -- 8 :+ 2
let differenz = z1 - z2  -- -2 :+ 6
let produkt = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- Komplex konjugiert
let konjugiertZ1 = conjugate z1  -- 3 :+ (-4)

-- Betrag und Phase
let betragZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- Polarkoordinaten zu kartesischen Koordinaten und umgekehrt
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let ausPolar = mkPolar 5.0 0.9272952180016122  -- entspricht z1
```

Beispielausgabe nach dem Laden des obigen Codes in GHCi könnte sein:

```haskell
*Main> summe
8.0 :+ 2.0
*Main> produkt
23.0 :+ 14.0
*Main> betragZ1
5.0
```

## Tiefergehende Betrachtung

Komplexe Zahlen reichen bis ins 16. Jahrhundert zurück, wurden aber viel später allgemein akzeptiert. Haskell bietet wie viele Sprachen eine native Unterstützung für komplexe Arithmetik, was das Arbeiten mit diesen Zahlen ohne die Implementierung der zugrunde liegenden Mathematik erleichtert.

Alternativen beinhalten den Aufbau Ihres eigenen komplexen Zahlentyps oder die Verwendung von Bibliotheken für spezifische Domänen, wie Quaternions für 3D-Grafiken. Aber für die meisten Anwendungsfälle ist Hasells `Data.Complex` ausreichend.

Im Kern ist `Data.Complex` nur ein Datentyp, der zwei `Float` oder `Double` Werte paart, die jeweils den Real- und Imaginärteil repräsentieren. Es ist eine unkomplizierte und effiziente Methode, um mit komplexen Zahlen auf der Haskell-Plattform zu arbeiten.

## Siehe auch

Schauen Sie sich diese Ressourcen für mehr über komplexe Zahlen in Haskell an:

- Die offizielle Haskell `Data.Complex` Dokumentation: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- Eine tiefergehende Betrachtung von Hasells Zahlentypen: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- Für eine Anwendung erkunden Sie Fast Fourier Transform-Algorithmen in Haskell: [Haskell FFT Bibliothek](https://hackage.haskell.org/package/fft)
