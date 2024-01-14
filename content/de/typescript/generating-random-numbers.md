---
title:                "TypeScript: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

Das Generieren von zufälligen Zahlen ist ein wichtiger Bestandteil der Programmierung und wird in vielen verschiedenen Anwendungen verwendet. Zum Beispiel können zufällige Zahlen in Spiel- oder Simulationsanwendungen verwendet werden, um ein dynamisches Verhalten zu erzeugen. Sie können auch in Kryptographie-Algorithmen eingesetzt werden, um Sicherheit zu gewährleisten. Das Verstehen, wie man zufällige Zahlen in TypeScript erzeugt, ist daher ein wesentlicher Schritt, um vielseitige und robuste Anwendungen zu entwickeln.

# Wie man zufällige Zahlen in TypeScript generiert

Das Erzeugen von zufälligen Zahlen in TypeScript ist relativ unkompliziert. TypeScript bietet eine eingebaute Methode, um zufällige Ganzzahlen zu generieren, die `Math.random()` genannt wird. Diese Methode gibt eine zufällige Dezimalzahl zwischen 0 und 1 zurück. Um eine Ganzzahl aus diesem Bereich zu erhalten, können wir sie mit `Math.floor()` multiplizieren. Hier ist ein Beispielcode, der eine zufällige Ganzzahl zwischen 1 und 10 generiert:

```TypeScript
const randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);
```

Dieser Code multipliziert den zufälligen Wert mit 10 und rundet ihn dabei auf die nächste Ganzzahl ab, um eine Zahl zwischen 0 und 10 zu erhalten. Dann fügt er 1 hinzu, um sicherzustellen, dass die Zahl mindestens 1 ist. Der Code gibt dann die generierte Zahl mit `console.log()` aus.

# Vertiefte Informationen über die Erzeugung von zufälligen Zahlen

Obwohl `Math.random()` eine praktische Methode ist, gibt sie immer noch nur eine Pseudozufallszahl zurück, die auf einem Algorithmus basiert. Wenn Sie also eine wirklich zufällige Zahl benötigen, z.B. für kryptografische Zwecke, sollten Sie eine externe Bibliothek verwenden, die speziell für diesen Zweck entwickelt wurde. Ebenfalls ist es wichtig zu beachten, dass die zufälligen Zahlen, die durch `Math.random()` generiert werden, nicht gleichmäßig verteilt sind. Wenn Sie also eine gleichmäßige Verteilung benötigen, sollten Sie eine andere Methode verwenden, z.B. die `crypto` API.

# Siehe auch

- [MDN Dokumentation zu Math.random()](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Zufall in der Programmierung](https://de.wikipedia.org/wiki/Zufall_in_der_Programmierung)