---
title:                "Javascript: Erzeugung von Zufallszahlen"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit der Generierung von Zufallszahlen beschäftigen? Die Antwort ist einfach: Zufallszahlen sind ein wichtiger Bestandteil vieler Programmierprojekte. Sie ermöglichen beispielsweise die Erstellung von zufälligen Charakteren in Computerspielen oder die Durchführung von statistischen Simulationen.

## Wie
Die Generierung von Zufallszahlen ist in Javascript sehr einfach und kann auf verschiedene Arten erfolgen. Eine Möglichkeit ist die Verwendung der Math.random() Funktion. Diese Funktion gibt eine zufällige Zahl zwischen 0 und 1 zurück. Um eine zufällige ganze Zahl zu generieren, kann man die Funktion mit der Methode Math.floor() kombinieren. Hier ein Beispiel:

```Javascript
let randomInt = Math.floor(Math.random() * 10); // gibt eine zufällige ganze Zahl zwischen 0 und 9 zurück
console.log(randomInt); // gibt den generierten Wert aus (z.B. 7)
```

Die generierten Zahlen können auch in Arrays gespeichert werden, um eine zufällige Auswahl daraus zu treffen. Hier ein Beispiel, das eine zufällige Farbe aus einem Array ausgibt:

```Javascript
let colors = ["rot", "gelb", "blau", "grün", "orange", "lila"];
let randomColor = Math.floor(Math.random() * colors.length);
console.log(colors[randomColor]); // gibt eine zufällige Farbe aus dem Array aus (z.B. "blau")
```

Es gibt auch Bibliotheken wie beispielsweise "random.js", die umfangreichere Funktionen zur Generierung von Zufallszahlen bieten.

## Deep Dive
Bei der Verwendung von Zufallszahlen ist es wichtig zu beachten, dass diese nicht wirklich zufällig sind, sondern auf vordefinierten Algorithmen beruhen. Es gibt daher keine Garantie für absolute Zufälligkeit. Auch kann es vorkommen, dass bei schnelleren Aufrufen derselben Funktion, scheinbar gleiche Zufallszahlen zurückgegeben werden.

Ein weiterer wichtiger Aspekt ist die "Saat" (Seed), die bei der Generierung von Zufallszahlen verwendet wird. Diese wird als Startwert für den Algorithmus verwendet und beeinflusst die darauffolgenden generierten Zahlen. Um beispielsweise stets dieselben Zufallszahlen zu erhalten, kann man einen festen Wert als Samen setzen. Andernfalls kann man auch einen dynamischen Wert wie zum Beispiel die aktuelle Uhrzeit verwenden, um eine größere Variabilität zu erzielen.

## Siehe auch
- https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- https://github.com/joelnet/random.js
- https://www.mathematik.de/geschichten-zahlung-hilfsbereite-zufallszahlen/