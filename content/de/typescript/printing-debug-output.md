---
title:                "Ausgabe von Debug-Informationen drucken."
html_title:           "TypeScript: Ausgabe von Debug-Informationen drucken."
simple_title:         "Ausgabe von Debug-Informationen drucken."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Debug-Ausgabe ist das Drucken von Informationen während der Ausführung eines Programms, um Probleme oder Unstimmigkeiten im Code zu identifizieren. Programmierer nutzen diese Methode, um Fehler zu finden und zu beheben und die Funktionalität ihres Codes zu überprüfen.

## Wie geht's:

```TypeScript 
// Beispiel 1: Einfache Debug-Ausgabe
console.log("Hello world!");

// Beispiel 2: Ausgabe einer Variablen
let name: string = "John";
console.log(`Hello ${name}`);

// Beispiel 3: Bedingte Debug-Ausgabe
let number: number = 5;
if (number > 10) {
    console.log("Die Zahl ist größer als 10.");
} else {
    console.log("Die Zahl ist kleiner als 10.");
}
```

Beispiel-Ausgabe:
```
Hello world!
Hello John
Die Zahl ist kleiner als 10.
```

## Tiefer in die Materie:

(1) Debug-Ausgabe wurde in den Anfängen des Computerzeitalters als nützliches Werkzeug für Programmierer entwickelt und ist auch heute noch eine wichtige Methode zur Fehlerbehebung.

(2) Eine Alternative zur Debug-Ausgabe ist das Setzen von Breakpoints im Code und das Durchlaufen des Programms Schritt für Schritt. Jedoch kann dies zeitaufwändiger sein als die Verwendung von Debug-Ausgabe, insbesondere bei größeren Programmen.

(3) In TypeScript gibt es mehrere Möglichkeiten, Debug-Ausgabe zu nutzen, zum Beispiel die integrierte Konsole oder spezielle Debugging-Tools in Entwicklungsumgebungen wie Visual Studio Code.

## Siehe auch:

- Offizielle Dokumentation zu Debug-Ausgabe in TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#the-new-and-improved-way-of-working-with-logging-functions
- Weitere Informationen zu Debugging-Techniken: https://codeburst.io/javascript-debugging-techniques-3cf5633361d5