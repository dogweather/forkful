---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:39:12.302213-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen sind eine Kombination aus reellen und imaginären Zahlen, wie `a + bi`, wobei `i` die Quadratwurzel von -1 ist. Sie sind entscheidend in Bereichen wie Ingenieurwesen und Physik, um Probleme zu lösen, an denen sich reguläre Zahlen die Zähne ausbeißen.

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
