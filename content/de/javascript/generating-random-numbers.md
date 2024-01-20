---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Zufallszahlen in JavaScript Erzeugen

## Was & Warum?
Zufallszahlen in Programmierung sind Zahlen, die von Programm erzeugt werden, ohne erkennbares Muster. Sie sind essentiell in verschiedenen Bereichen, wie z.B. Spiele, Simulationen oder kryptographische Anwendungen, um Unvorhersehbarkeit und Abwechslung zu erzeugen.

## So Geht's:
In JavaScript können wir die `Math.random()` Funktion nutzen, um eine Zufallszahl zwischen 0 (einschließlich) und 1 (ausschließlich) zu erzeugen.

```Javascript
let zufallszahl = Math.random();
console.log(zufallszahl);
```

Wir können auch eine Funktion schreiben, um eine Zufallszahl in einem bestimmten Bereich zu erzeugen. 

```Javascript
function getRandom(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandom(1, 10));  // Gibt eine Zufallszahl zwischen 1 und 10 aus.
```

## Tiefer Einblick
Das Erzeugen von Zufallszahlen hat eine lange Geschichte in der Informatik, die weit vor dem Aufkommen von Hochsprachen wie JavaScript liegt. Die Methode `Math.random()` in JavaScript ist eine Verbindung zu dieser Geschichte und basiert auf einem Pseudozufallszahlengenerator.

Im Gegensatz zu echten Zufallszahlen werden Pseudozufallszahlen durch einen anfänglichen Wert, bekannt als Samen oder "seed", und einen festen algorithmischen Prozess erzeugt. Sie sind nicht wirklich "zufällig" im strengen Sinne, aber für die meisten Anforderungen sind sie ausreichend.

Es gibt auch alternative Methode zum Erzeugen von Zufallszahlen, wie die Durchführung einer kryptographisch sicheren Pseudozufallszahlengenerator (CSPRNG) mit dem `crypto.getRandomValues()` Methode.

## Siehe auch
Für weitere Informationen und Lernressourcen, siehe die folgenden Links:

- [MDN Web-Dokumentation zu `Math.random()`](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web-Dokumentation zu `crypto.getRandomValues()`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [Mehr über Pseudozufallszahlengenerator](https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator)