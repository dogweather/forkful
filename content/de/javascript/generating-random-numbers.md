---
title:                "Zufällige Zahlen generieren"
html_title:           "Javascript: Zufällige Zahlen generieren"
simple_title:         "Zufällige Zahlen generieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum es überhaupt wichtig ist, zufällige Zahlen zu generieren. Nun, es gibt viele Anwendungsfälle, in denen zufällige Zahlen unerlässlich sind, z.B. in der Spieleentwicklung, beim Erstellen von Passwörtern oder beim Testen von Algorithmen. Es ist also eine wichtige Fähigkeit in der Welt der Programmierung.

## Wie man zufällige Zahlen generiert

Um zufällige Zahlen in Javascript zu generieren, gibt es zwei Methoden: `Math.random()` und `crypto.getRandomValues()`. Lass uns jeden Schritt des Prozesses im folgenden Codeblock genauer betrachten:

```Javascript
// Methode 1: Math.random()
const random1 = Math.random(); // gibt eine zufällige Zahl zwischen 0 und 1 zurück
console.log(random1); // z.B. 0.5649519366257007

// Methode 2: crypto.getRandomValues()
let buffer = new Uint32Array(1); // erstellt einen Puffer für eine 32-Bit-Zahl
crypto.getRandomValues(buffer); // füllt den Puffer mit zufälligen Werten
const random2 = buffer[0] / (Math.pow(2, 32) - 1); // berechnet eine zufällige Zahl zwischen 0 und 1
console.log(random2); // z.B. 0.8107146544839467
```

Wie du sehen kannst, gibt `Math.random()` einfach eine zufällige Dezimalzahl zurück, während `crypto.getRandomValues()` etwas komplexer ist. Die zweite Methode ist jedoch sicherer, da sie auf kryptographisch sicheren Pseudozufallszahlen basiert.

## Tiefere Einblicke

Um besser zu verstehen, wie zufällige Zahlen in Javascript generiert werden, ist es wichtig zu verstehen, dass es keine 100%ig zufällige Zahl gibt. Jede Methode basiert auf einer gewissen Logik und Algorithmus, um eine Scheinzufälligkeit zu erzeugen. Diese Algorithmen können jedoch immer weiter verbessert werden, um die Zufälligkeit zu erhöhen.

Ein weiterer wichtiger Punkt ist, dass es für diese Methoden wichtig ist, eine sogenannte "Seed"-Zahl als Startpunkt zu haben. Diese Seed-Zahl wird verwendet, um die Berechnung von zufälligen Zahlen zu beeinflussen und so unterschiedliche Resultate zu erzeugen. Ohne eine Seed-Zahl würde jede zufällige Generierung immer die gleiche Zahl liefern.

## Siehe auch

- [Zufällige Zahlen in Javascript generieren (MDN)](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Kryptographisch sichere Zufallszahlen in Javascript generieren (MDN)](https://developer.mozilla.org/de/docs/Web/API/Crypto/getRandomValues)
- [Differentiating between random number generators in Javascript (Stack Overflow)](https://stackoverflow.com/questions/42532743/differentiating-between-random-number-generators-in-javascript)