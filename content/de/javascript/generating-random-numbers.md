---
title:    "Javascript: Zufällige Zahlen generieren"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Erzeugung von zufälligen Zahlen befassen? Nun, es gibt unzählige Anwendungen, bei denen die Verwendung von Zufallszahlen unerlässlich ist. Von der Simulation von Daten und Ereignissen bis zur Verschlüsselung von sensiblen Informationen, die Generierung von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung und kann vielseitig eingesetzt werden.

## Wie

Die Erzeugung von Zufallszahlen in Javascript ist relativ einfach. Es gibt viele verschiedene Methoden und Bibliotheken, die verwendet werden können. Hier ein Beispiel mit der Standard-Math.random()-Funktion:

```Javascript
// Generierung einer Zufallszahl zwischen 0 und 1
let random = Math.random();
console.log(random); // Ausgabe: 0.5928374658
```

Diese Methode gibt eine Zufallszahl zwischen 0 (inklusive) und 1 (exklusive) aus. Wenn wir also eine ganze Zahl zwischen 1 und 10 benötigen, können wir einfach die Funktion mit 10 multiplizieren und anschließend aufrunden:

```Javascript
// Generierung einer Zufallszahl zwischen 1 und 10
let random = Math.random() * 10;
console.log(Math.ceil(random)); // Ausgabe: eine ganze Zahl zwischen 1 und 10, z.B. 7
```

Um eine ganze Zahl zwischen einem bestimmten Minimum und Maximum zu erhalten, können wir die Zufallszahl entsprechend skalieren:

```Javascript
// Generierung einer ganzen Zahl zwischen 5 und 15
let random = (Math.random() * 10) + 5;
console.log(Math.floor(random)); // Ausgabe: eine ganze Zahl zwischen 5 und 15, z.B. 11
```

Es gibt auch Bibliotheken, wie z.B. die "random-js" Bibliothek, die noch mehr Funktionen und Optionen für die Erzeugung von Zufallszahlen bietet.

## Deep Dive

Die Erzeugung von Zufallszahlen mag einfach erscheinen, aber es ist wichtig zu verstehen, dass Zufallszahlen in der Programmierung eigentlich nicht wirklich "zufällig" sind. Sie werden durch komplexe Algorithmen generiert, die auf bestimmte Eingangsparameter, wie zum Beispiel die Systemzeit, zugreifen.

Ein weiterer wichtiger Aspekt der Generierung von Zufallszahlen ist ihre Gleichverteilung. Das bedeutet, dass die Wahrscheinlichkeiten für das Auftreten einer bestimmten Zufallszahl gleich sind. Wenn wir also eine Zufallszahl zwischen 1 und 10 erwarten, sollte jede dieser Zahlen eine Wahrscheinlichkeit von 1/10 haben, aufzutreten.

Wenn wir uns mit kryptographisch sicheren Zufallszahlen befassen, also Zufallszahlen, die nicht vorhersehbar sind und für die Keine-Konstruktionsmethode verwendet werden können, muss ein speziell entwickelter Algorithmus verwendet werden, der eine hohe Entropie garantiert. Hier ist es wichtig, sich von Standard-Zufallszahlengeneratoren zu distanzieren und spezifische Methoden zu verwenden, die für eine maximale Sicherheit sorgen.

## Siehe Auch

- [Math.random() Dokumentation von Mozilla](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Random-JS Bibliothek](https://www.npmjs.com/package/random-js)
- [Kryptographisch sichere Zufallszahlen in Javascript](https://stackoverflow.com/questions/424292/seedable-javascript-random-number-generator)
- [Entropie in der Kryptographie](https://de.wikipedia.org/wiki/Entropie_(Kryptographie))