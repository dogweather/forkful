---
date: 2024-01-26 03:49:55.091778-07:00
description: "Die Verwendung eines Debuggers bedeutet, spezialisierte Werkzeuge zu\
  \ nutzen, mit denen man unter die Haube seines Codes schauen und ihn Schritt f\xFC\
  r\u2026"
lastmod: '2024-03-13T22:44:54.270630-06:00'
model: gpt-4-0125-preview
summary: "Die Verwendung eines Debuggers bedeutet, spezialisierte Werkzeuge zu nutzen,\
  \ mit denen man unter die Haube seines Codes schauen und ihn Schritt f\xFCr\u2026"
title: Einsatz eines Debuggers
---

## Wie geht das:
Hier ein Stück JavaScript-Code, das sich nicht wie erwartet verhält:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Hoppla! Das sollte eine Multiplikation sein, keine Addition.
}

let result = buggyMultiply(5, 3);
console.log('Ergebnis:', result);
```

Die Ausgabe ist falsch:
```
Ergebnis: 8
```

Lassen Sie uns in den Chrome DevTools debuggen:

1. Öffnen Sie dieses JS in einem Browser.
2. Rechtsklick und "Untersuchen" auswählen, um DevTools zu öffnen.
3. Klicken Sie auf den Reiter "Quellen".
4. Finden Sie Ihren Codeausschnitt oder die Seite und setzen Sie einen Haltepunkt, indem Sie auf die Zeilennummer neben der `return`-Anweisung klicken.
5. Aktualisieren Sie die Seite, um den Haltepunkt auszulösen.
6. Überprüfen Sie das "Scope"-Panel, um die lokalen Variablen `a` und `b` zu sehen.
7. Schreiten Sie mit dem Button "Nächsten Funktionsaufruf überspringen" voran.
8. Finden Sie den Fehler in der `return`-Anweisung.
9. Korrigieren Sie den Code:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Korrigiert!
}

let result = buggyMultiply(5, 3);
console.log('Ergebnis:', result);
```

Die korrigierte Ausgabe:
```
Ergebnis: 15
```

## Vertiefung
Das Konzept des Debuggens gibt es schon seit den Anfangstagen der Computertechnik – die Legende besagt, dass es begann, als ein Nachtfalter in einem Computer in den 1940ern gefunden wurde! Heute bieten JavaScript-Debugger wie die in den Browser integrierten Werkzeuge (Chrome DevTools, Firefox Developer Tools) oder IDE-integrierten Debugger (Visual Studio Code, WebStorm) eine Vielzahl von Funktionen.

Alternativen zu integrierten Debuggern umfassen Drittanbieter-Tools wie WebStorm oder das gute alte `console.log`, um Variablenzustände auszugeben. Aber diese bieten nicht die Echtzeit-Interaktion und detaillierte Inspektion, die Debugger bieten.

Bezüglich der Implementierungsdetails arbeiten die meisten Debugger ähnlich: Sie erlauben es Ihnen, Haltepunkte zu setzen, die die Ausführung pausieren, Schritt für Schritt durch den Code zu gehen, den aktuellen Zustand von Variablen zu untersuchen, Ausdrücke zu beobachten und sogar Werte unterwegs zu manipulieren, um unterschiedliche Szenarien zu testen.

## Siehe auch
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/de/docs/Tools/Debugger)
- [Visual Studio Code - Debugging](https://code.visualstudio.com/docs/editor/debugging)
