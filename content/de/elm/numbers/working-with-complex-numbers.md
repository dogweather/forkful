---
date: 2024-01-26 04:39:12.302213-07:00
description: "Wie geht das: Elm unterst\xFCtzt komplexe Zahlen nicht direkt, daher\
  \ m\xFCssen Sie Ihren eigenen Typ und Ihre eigenen Funktionen erstellen. Hier eine\
  \ kurze\u2026"
lastmod: '2024-03-13T22:44:53.798766-06:00'
model: gpt-4-0125-preview
summary: "Elm unterst\xFCtzt komplexe Zahlen nicht direkt, daher m\xFCssen Sie Ihren\
  \ eigenen Typ und Ihre eigenen Funktionen erstellen."
title: Umgang mit komplexen Zahlen
weight: 14
---

## Wie geht das:
Elm unterstützt komplexe Zahlen nicht direkt, daher müssen Sie Ihren eigenen Typ und Ihre eigenen Funktionen erstellen. Hier eine kurze Einrichtung:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Beispielverwendung:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum ist { real = 4.0, imaginary = -2.0 }
```

## Tiefer Eintauchen
Historisch gesehen wurden komplexe Zahlen nicht immer akzeptiert. Im 16. Jahrhundert wurden sie jedoch zum Game-Changer, um kubische Gleichungen zu lösen. Alternativen in anderen Sprachen wie Python bieten eingebaute Unterstützung für komplexe Zahlen mit direkt verfügbaren Operationen. Wie Sie gesehen haben, erfordert Elm einen DIY-Ansatz. Aber Sie können es so anspruchsvoll gestalten wie nötig, indem Sie Multiplikation, Division und andere Operationen aufbauen und Leistungsprobleme beheben.

## Siehe auch
- Elms Offizielle Dokumentation: https://package.elm-lang.org/ zum Erstellen benutzerdefinierter Typen und zum Meistern der Elm-Grundlagen.
- Fans der Mathematikgeschichte könnten einen Blick in "An Imaginary Tale" von Paul J. Nahin werfen, um die Reise der komplexen Zahlen durch die Zeit zu verfolgen.
- Tauchen Sie in mathematikorientierte Programmierherausforderungen auf Project Euler (https://projecteuler.net) ein, um Ihre Zauberei mit komplexen Zahlen anzuwenden.
