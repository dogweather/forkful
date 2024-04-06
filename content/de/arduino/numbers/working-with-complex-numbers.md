---
date: 2024-01-26 04:36:51.380777-07:00
description: 'Wie: Beispielausgabe.'
lastmod: '2024-04-05T21:53:56.023036-06:00'
model: gpt-4-0125-preview
summary: ''
title: Umgang mit komplexen Zahlen
weight: 14
---

## Wie:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Starte serielle Kommunikation
  
  Complex myComplex(2, 3); // Erstelle eine komplexe Zahl 2 + 3i
  Complex anotherComplex(1, 1); // Erstelle eine weitere komplexe Zahl 1 + 1i
  
  // Addition
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addition: "); 
  result.print(); // Gibt aus 3 + 4i
  
  // Multiplikation
  result = myComplex * anotherComplex; 
  Serial.print("Multiplikation: ");
  result.print(); // Gibt aus -1 + 5i
}

void loop() {
  // Wird in diesem Beispiel nicht verwendet
}
```
Beispielausgabe:
```
Addition: 3 + 4i
Multiplikation: -1 + 5i
```

## Vertiefung
Ursprünglich wurden komplexe Zahlen mit Skepsis betrachtet, aber sie sind zentral in verschiedenen wissenschaftlichen Feldern geworden. Historisch gesehen wurden sie dafür anerkannt, Lösungen für Polynomgleichungen zu bieten, denen es an realen Lösungen fehlt.

Arduino enthält keine komplexen Zahlen in seiner Standardbibliothek, aber Sie können Bibliotheken wie `Complex.h` nutzen, um damit umzugehen. Intern definieren diese Bibliotheken eine Complex-Klasse, die typischerweise zwei Doubles verwendet, um den Real- und Imaginärteil zu speichern, und Operatoren überladen, um Arithmetik zu unterstützen.

Alternativ, für Anwendungen, die keine komplexe Zahlenarithmetik inhärent benötigen, erwägen Sie, andere mathematische Strategien oder Bibliotheken zu verwenden. Denken Sie jedoch daran, dass die Verwendung von Fließkommazahlen anstelle von komplexen Zahlen einige Probleme vereinfachen könnte.

## Siehe auch
- Die [Complex.h](https://github.com/RobTillaart/Complex) Bibliothek von Rob Tillaart.
- Ein tieferer Einblick in die [Mathematik hinter komplexen Zahlen](https://mathworld.wolfram.com/ComplexNumber.html).
