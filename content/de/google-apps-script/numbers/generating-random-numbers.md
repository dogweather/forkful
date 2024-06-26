---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:49.071660-07:00
description: "Wie geht das: In Google Apps Script k\xF6nnen Sie Zufallszahlen mit\
  \ der Funktion `Math.random()` generieren, \xE4hnlich wie in JavaScript. Diese Funktion\
  \ gibt\u2026"
lastmod: '2024-03-13T22:44:53.329339-06:00'
model: gpt-4-0125-preview
summary: "In Google Apps Script k\xF6nnen Sie Zufallszahlen mit der Funktion `Math.random()`\
  \ generieren, \xE4hnlich wie in JavaScript."
title: Zufallszahlen generieren
weight: 12
---

## Wie geht das:
In Google Apps Script können Sie Zufallszahlen mit der Funktion `Math.random()` generieren, ähnlich wie in JavaScript. Diese Funktion gibt eine Fließkommazahl, pseudozufällige Zahl im Bereich 0 (einschließlich) bis 1 (ausschließlich) zurück. Um diese Zahlen für verschiedene Anwendungsfälle anzupassen, wie das Generieren von Ganzzahlen in einem bestimmten Bereich, müssen Sie möglicherweise zusätzliche Berechnungen durchführen.

### Eine einfache Zufallszahl generieren
Um eine einfache Zufallszahl zu generieren und sie auf der Konsole zu protokollieren:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Beispielausgabe:* `0.1234567890123456`

### Eine Ganzzahl innerhalb eines bestimmten Bereichs generieren
Um eine zufällige Ganzzahl zwischen zwei Werten (`min` und `max`), einschließlich, zu generieren:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Beispiel:
getRandomInt(1, 10);
```
*Beispielausgabe*: `7`

Denken Sie daran, die Funktion `Math.ceil()` wird verwendet, um den Mindestwert aufzurunden, und `Math.floor()` wird verwendet, um den Höchstwert abzurunden, um sicherzustellen, dass die Zufallszahl innerhalb des angegebenen Bereichs liegt.

## Tiefer eintauchen
Der Mechanismus zur Generierung von Zufallszahlen in Google Apps Script und in den meisten Programmiersprachen verwendet einen pseudozufälligen Zahlengenerator (PRNG). Diese Technik ist deterministisch und basiert auf einem Anfangswert, bekannt als der Seed, um eine Abfolge von Zahlen zu erzeugen, die zufällig erscheint. Obwohl dies für viele Anwendungen ausreichend ist, ist es wichtig zu beachten, dass pseudozufällige Zahlen möglicherweise nicht geeignet sind, wo hohe Sicherheit oder echte Zufälligkeit erforderlich ist, wie bei kryptografischen Anwendungen.

Echte Zufälligkeit kann durch Hardware-Zufallszahlengeneratoren oder Dienste erreicht werden, die Zufälligkeit aus natürlichen Phänomenen generieren. Allerdings reicht für die meisten alltäglichen Scripting-Bedürfnisse in Google Apps Script `Math.random()` aus.

Historisch gesehen hat die Suche nach effektiveren Techniken zur Generierung von Zufallszahlen zur Entwicklung verschiedener Algorithmen geführt, mit bemerkenswerten Beispielen wie dem Mersenne Twister und dem Linearen Kongruenzgenerator (LCG). Angesichts der hohen Abstraktionsebene in Google Apps Script müssen die meisten Benutzer diese Algorithmen jedoch nicht direkt implementieren, aber das Verständnis der zugrundeliegenden Prinzipien kann helfen, die Bedeutung und die Grenzen der Zufallszahlengenerierung in Ihren Skripten zu schätzen.
