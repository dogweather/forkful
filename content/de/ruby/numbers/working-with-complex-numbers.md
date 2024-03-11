---
date: 2024-01-26 04:45:10.098871-07:00
description: "Komplexe Zahlen, bestehend aus einem realen und einem imagin\xE4ren\
  \ Teil (wie 3+4i), sind ein Grundpfeiler in der Ingenieurwissenschaft und Physik.\u2026"
lastmod: '2024-03-11T00:14:28.304257-06:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen, bestehend aus einem realen und einem imagin\xE4ren Teil\
  \ (wie 3+4i), sind ein Grundpfeiler in der Ingenieurwissenschaft und Physik.\u2026"
title: Umgang mit komplexen Zahlen
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen, bestehend aus einem realen und einem imaginären Teil (wie 3+4i), sind ein Grundpfeiler in der Ingenieurwissenschaft und Physik. Programmierer arbeiten bei Simulationen, Signalverarbeitung und der Lösung von Gleichungen, die mit rein reellen Zahlen nicht gut funktionieren, mit ihnen.

## Wie geht das:
Ruby macht den Umgang mit komplexen Zahlen kinderleicht. Man kann sie mit der Klasse Complex erstellen und manipulieren:

```ruby
require 'complex'

# Komplexe Zahlen erstellen
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Grundoperationen
summe = c1 + c2               # => (5.0+9.0i)
differenz = c1 - c2        # => (1.0-1.0i)
produkt = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Konjugiert, Betrag, und Phase
konjugiert = c1.conjugate    # => (3.0-4.0i)
betrag = c1.abs          # => 5.0
phase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 Radiant

# Spezifische Methoden für komplexe Zahlen
polar = c1.polar            # => [5.0, 0.9272952180016122]
rechteckig = c1.rect       # => [3.0, 4.0]
```

## Vertiefung
Komplexe Zahlen sind nicht neu – sie gibt es seit dem 16. Jahrhundert und lösen Gleichungen, die keine reellen Lösungen haben. Abseits der Mathematik übernimmt in der Programmierung die Complex-Klasse von Ruby die schweren Berechnungen, unterstützt vom Math-Modul für trigonometrische und transzendente Funktionen.

Frühere Programmiersprachen erforderten eine manuelle Handhabung von realem und imaginärem Teil. Einige, wie Fortran und C++, widmen spezielle Bibliotheken der komplexen Arithmetik.

Rubys Ansatz bettet die Unterstützung für komplexe Zahlen direkt in seine Syntax ein, sodass man das Rad nicht neu erfinden muss. Hinter den Kulissen übernimmt die Complex-Klasse die Mathematik, während Ruby die Interaktionen zwischen Objekten regelt.

## Siehe auch
- Ruby-Dokumentation zu Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- MathWorlds Sicht auf komplexe Zahlen: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Eine visuelle Einführung in komplexe Zahlen und warum sie nützlich sind: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
