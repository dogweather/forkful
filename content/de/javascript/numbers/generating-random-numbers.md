---
title:                "Generierung von Zufallszahlen"
aliases:
- /de/javascript/generating-random-numbers/
date:                  2024-01-27T20:34:35.060108-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in JavaScript ist eine Technik, die verwendet wird, um Unvorhersehbarkeit in Anwendungen zu schaffen, von Spielen, die ein zufälliges Verhalten von Gegnern benötigen, bis hin zu Sicherheitsalgorithmen, die kryptografische Zufälligkeit erfordern. Diese Fähigkeit ist entscheidend für die Entwicklung dynamischer Benutzererfahrungen und sicherer Anwendungen.

## Wie geht das:

### Grundlegende Zufallszahlengenerierung

Die einfachste Möglichkeit, eine Zufallszahl in JavaScript zu generieren, besteht darin, `Math.random()` zu verwenden. Diese Funktion gibt eine Gleitkommazahl, eine pseudo-zufällige Zahl im Bereich von 0 (einschließlich) bis 1 (ausschließlich), zurück.

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generierung einer Zufallszahl innerhalb eines Bereichs

Oft möchte man eine ganze Zufallszahl innerhalb eines bestimmten Bereichs. Dies kann erreicht werden, indem man die Ausgabe von `Math.random()` skaliert und rundet.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Kryptografisch sichere Zufallszahlen

Für Anwendungen, die einen höheren Grad an Zufälligkeit erfordern (z. B. kryptografische Operationen), kann die Methode `crypto.getRandomValues()` verwendet werden. Diese bietet kryptografische Zufälligkeit, im Gegensatz zu den pseudo-zufälligen Zahlen, die von `Math.random()` generiert werden.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Tiefergehend

Historisch gesehen war die Generierung von Zufallszahlen in JavaScript ausschließlich auf die Funktion `Math.random()` angewiesen. Sie ist zwar bequem für die meisten gelegentlichen Anwendungsfälle, ihr Algorithmus, typischerweise eine Variante eines Pseudorandom-Number-Generator (PRNG) wie Mersenne Twister, bietet jedoch keine kryptografische Sicherheit.

Die Einführung der Web Cryptography API brachte die Methode `crypto.getRandomValues()`, die eine Möglichkeit bietet, Zahlen zu generieren, die weit weniger vorhersehbar und für sicherheitssensitive Anwendungen geeignet sind. Diese Methode greift auf die Zufälligkeitsquellen des zugrunde liegenden Betriebssystems zurück, wie `/dev/random` auf Unix/Linux, die robuster sind und sich besser für kryptografische Operationen eignen.

Es ist entscheidend, die richtige Methode für die anstehende Aufgabe zu wählen. `Math.random()` reicht für grundlegende Bedürfnisse wie einfache Spiele, Animationen oder jeden Fall, in dem die Qualität der Zufälligkeit nicht kritisch ist. Für Sicherheitsfunktionen wie Passwort-Reset-Token oder jegliche kryptografische Operationen ist jedoch `crypto.getRandomValues()` die bessere Wahl aufgrund seiner überlegenen Zufälligkeitsqualität.

Beachtenswert ist, dass `Math.random()` in den meisten Implementierungen Zahlen mit einer bekannten Verzerrung generiert, was bedeutet, dass einige Zahlen wahrscheinlicher auftreten als andere. Auch wenn diese Verzerrung minimal und oft für allgemeine Anwendungen unmerklich ist, disqualifiziert sie `Math.random()` von der Verwendung in einem kryptografischen Kontext oder Anwendungen, bei denen Fairness entscheidend ist, wie Online-Glücksspiel.

Zusammenfassend wird, während JavaScripts integrierte Funktionen zur Generierung von Zufallszahlen eine breite Palette von Bedürfnissen abdecken, das Verständnis der Unterschiede und Einschränkungen jeder Methode für ihren angemessenen Einsatz wesentlich.
