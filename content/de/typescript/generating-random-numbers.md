---
title:                "Zufallszahlen erzeugen"
html_title:           "TypeScript: Zufallszahlen erzeugen"
simple_title:         "Zufallszahlen erzeugen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum? 
Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung. Es ermöglicht uns, zufällige Ereignisse zu simulieren oder eine Komponente des Codes unvorhersehbar zu machen. Programmierer nutzen dies, um Spiele zu erstellen, Verschlüsselungsalgorithmen zu verbessern oder Tests zu automatisieren, die auf verschiedene Eingaben reagieren müssen.

## Wie geht's?
Das Erstellen von Zufallszahlen in TypeScript ist einfach: Wir verwenden einfach die eingebaute Methode ```Math.random ()```, um eine zufällige Dezimalzahl zwischen 0 und 1 zu erhalten. Um sicherzustellen, dass die Zahl im gewünschten Bereich liegt, können wir sie mit der Methode ```Math.floor ()``` auf die nächste ganze Zahl abrunden und sie mit dem gewünschten Bereich multiplizieren.

```TypeScript
let randomNumber: number = Math.floor(Math.random() * 100); // Rundet eine zufällige Dezimalzahl zwischen 0-1 auf die nächste ganze Zahl ab und multipliziert sie mit 100, um eine Zufallszahl zwischen 0 und 100 zu erhalten.
console.log(randomNumber); // Ausgabe: zufällige Zahl zwischen 0 und 100
```

## Tiefgehender Einblick
Zufallszahlen wurden erstmals im 17. Jahrhundert vom französischen Mathematiker Blaise Pascal untersucht, als er versuchte, ein Problem des Glücksspiels zu lösen. Es gibt auch alternative Methoden zum Generieren von Zufallszahlen, wie z.B. die Verwendung von Hardware-Geräten oder das Erstellen von Zufallszahlen aus einer Liste von vordefinierten Zahlen, um eine gleichmäßigere Verteilung zu erreichen. Die Implementierung der ```Math.random ()```-Methode in TypeScript basiert auf dem Mersenne-Twister-Algorithmus.

## Siehe auch
- [Math.random() Dokumentation](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Pseudozufallszahlengenerator](https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator)
- [Mersenne-Twister-Algorithmus](https://de.wikipedia.org/wiki/Mersenne-Twister)