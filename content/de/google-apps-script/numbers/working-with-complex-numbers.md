---
title:                "Arbeiten mit komplexen Zahlen"
aliases: - /de/google-apps-script/working-with-complex-numbers.md
date:                  2024-02-01T22:07:32.608222-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit komplexen Zahlen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen, die als Kombination aus reellen und imaginären Einheiten dargestellt werden (z.B. 3 + 4i), sind grundlegend für verschiedene Berechnungsprobleme, insbesondere in Ingenieurwesen, Physik und angewandter Mathematik. Das Erlernen der Manipulation dieser Zahlen in Google Apps Script ermöglicht es Programmierern, ihre Fähigkeiten auf wissenschaftliches Rechnen, Signalverarbeitung und darüber hinaus auszudehnen.

## Wie:
Google Apps Script bietet keine integrierte Unterstützung für komplexe Zahlen, was die Implementierung von benutzerdefinierten Funktionalitäten erforderlich macht. Unten ist eine grundlegende Struktur für den Umgang mit komplexen Zahlen dargestellt, einschließlich Addition, Subtraktion und Multiplikation.

```javascript
// Definiere einen Konstruktor für komplexe Zahlen
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Methode für die Addition von zwei komplexen Zahlen
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Methode für die Subtraktion von zwei komplexen Zahlen
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Methode für die Multiplikation von zwei komplexen Zahlen
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Beispielverwendung
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Zwei komplexe Zahlen addieren
var sum = num1.add(num2);
console.log(`Summe: ${sum.real} + ${sum.imag}i`); // Summe: 4 + 6i

// Zwei komplexe Zahlen subtrahieren
var difference = num1.subtract(num2);
console.log(`Differenz: ${difference.real} + ${difference.imag}i`); // Differenz: 2 + 2i

// Zwei komplexe Zahlen multiplizieren
var product = num1.multiply(num2);
console.log(`Produkt: ${product.real} + ${product.imag}i`); // Produkt: -5 + 10i
```

## Vertiefung:
Das Konzept der komplexen Zahlen reicht zurück bis ins 16. Jahrhundert, aber es waren die Arbeiten von Mathematikern wie Euler und Gauss, die ihren Platz in der Mathematik festigten. Trotz ihrer Nützlichkeit werden komplexe Zahlen weder in JavaScript noch in Google Apps Script direkt unterstützt. Das Fehlen einer nativen Unterstützung bedeutet, dass Operationen mit komplexen Zahlen manuell implementiert werden müssen, wie demonstriert. Obwohl dies eine gute Lernmöglichkeit bietet und ausreichende Funktionalitäten für grundlegende Bedürfnisse bereitstellt, sollte für schwere rechnerische Arbeiten, die komplexe Zahlen erfordern, in Betracht gezogen werden, andere Programmierumgebungen zu nutzen, die besser für mathematisches Computing geeignet sind, wie Python mit NumPy, die eingebaute, hoch optimierte Operationen für den Umgang mit komplexen Zahlen bieten. Dennoch ist das Verstehen und Implementieren grundlegender Operationen in Google Apps Script eine nützliche Übung für diejenigen, die ihre Programmierfähigkeiten erweitern und in einer breiten Palette von Kontexten anwenden möchten.
