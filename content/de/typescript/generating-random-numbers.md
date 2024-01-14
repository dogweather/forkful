---
title:                "TypeScript: Erzeugung von Zufallszahlen"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist oft ein wichtiger Bestandteil der Programmierung. Mit TypeScript können wir dieses Konzept auf einfache Weise implementieren und in unseren Code integrieren. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie das funktioniert und welche Möglichkeiten es gibt.

## Wie man Zufallszahlen in TypeScript generiert

Um Zufallszahlen in TypeScript zu generieren, müssen wir zuerst ein Objekt des Typs `Math` erstellen. Dieses Objekt enthält bereits eine Methode namens `random()`, die eine Zufallszahl zwischen 0 und 1 zurückgibt. Um eine höhere Zahl zu erhalten, können wir diese Methode mit der gewünschten Maximalzahl multiplizieren.

```TypeScript
let randomNumber: number = Math.random() * 100; // Gibt eine Zufallszahl zwischen 0 und 100 zurück
console.log(randomNumber);
```

Wir können auch eine minimale Zahl angeben, indem wir der `random()`-Methode die minimale Zahl hinzufügen und die gewünschte Differenz zwischen der maximalen und minimalen Zahl addieren.

```TypeScript
let randomNumber: number = Math.random() * (100 - 50) + 50; // Gibt eine Zufallszahl zwischen 50 und 100 zurück
console.log(randomNumber);
```

Wir können auch Zufallszahlen mit dem Datentyp `integer` generieren, indem wir die `random()`-Methode mit `floor()` oder `round()` kombinieren, um die Nachkommastellen abzuschneiden oder zu runden.

```TypeScript
let randomNumber: number = Math.floor(Math.random() * 100); // Gibt eine Zufallszahl zwischen 0 und 100 (exklusive) zurück
console.log(randomNumber);
```

## Tiefergehende Informationen zum Generieren von Zufallszahlen

Die `random()`-Methode in TypeScript verwendet den sogenannten "Linear Congruential Generator" Algorithmus, um Pseudo-Zufallszahlen zu generieren. Dies bedeutet, dass die erzeugten Zahlen nicht wirklich zufällig sind, sondern auf einer mathematischen Formel basieren. Es ist wichtig zu beachten, dass bei jedem Programmstart dieselben Zufallszahlen generiert werden, es sei denn, wir ändern den sogenannten "Seed" (Samen) des `Math`-Objekts.

Wir können auch eine eigene Funktion erstellen, um Zufallszahlen basierend auf einem anderen Algorithmus zu generieren. Zum Beispiel können wir den Mersenne Twister Algorithmus verwenden, der als einer der besten Zufallszahlengeneratoren gilt. Wir können Bibliotheken wie "random-js" oder "seedrandom" verwenden, um diesen Algorithmus in unseren TypeScript-Code zu implementieren.

## Siehe auch

- [TypeScript Dokumentation zu Math Funktionen](https://www.typescriptlang.org/docs/handbook/standard-library.html#math-functions)
- [Mersenne Twister Algorithmus in TypeScript implementieren](https://github.com/MattMcFarland/seedrandom)