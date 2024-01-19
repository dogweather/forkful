---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was und Warum?

Debug-Ausgaben drucken ist, wenn wir Daten, Variablen und Werte zur Laufzeit auf unserem Console oder Debug-Interface anzeigen. Wir machen das, um Programmfehler zu finden und das Verhalten des Programms besser zu verstehen.

## Anleitung

In TypeScript können Sie mithilfe der console.log() Funktion Debug-Ausgaben drucken. Sehen wir uns ein Beispiel an:

```TypeScript
let variable = 'Hallo Welt!';
console.log(variable);
```

Die Ausgabe wäre:

```
Hallo Welt!
```

## Vertiefung

Die Verwendung von Debug-Ausgaben ist alte Praxis, wird aber immer noch weithin akzeptiert und genutzt. Zunächst haben Programmierer mit LED-Blinkcodes gearbeitet, um Zustände zu übermitteln. Mit dem Fortschreiten der Technologie sind wir bei umfangreichen Konsolenmeldungen angelangt.

Alternative Methoden zur Fehlersuche sind der Step-by-Step Debugger und Profiling-Tools. Diese bieten detailreiche Einblicke, aber manchmal ist die Verwendung von console.log schneller und unkomplizierter, besonders in großen Projekten.

In TypeScript ist console.log auf JavaScript zurückzuführen. JavaScript hat seinen Ursprung im Web und ermöglicht den Zugriff auf das Web-Console-Feature der Browsers.

## Weiterführende Informationen 

- [MDN Web Docs: console.log()](https://developer.mozilla.org/en-US/docs/Web/API/console/log) 
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [The Basics of Debugging in JavaScript](https://www.digitalocean.com/community/tutorials/javascript-debugging)
- [Step-by-Step Debugging with Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)