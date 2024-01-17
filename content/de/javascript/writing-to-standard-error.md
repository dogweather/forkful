---
title:                "Schreiben auf Standardfehler"
html_title:           "Javascript: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Was ist "writing to standard error" und warum machen Programmierer das?
Beim Programmieren gibt es oft Fehler oder unerwartetes Verhalten, die es zu debuggen und beheben gilt. Das Schreiben von Fehlern auf die Standardfehlerausgabe ist eine Möglichkeit, diese Probleme zu erkennen und zu untersuchen. Programmierer verwenden dies als Diagnosewerkzeug, um den Code zu verbessern und Fehler zu beheben.

Wie funktioniert es?
Der Standardfehlerausgabestrom ist ein spezieller Ausgabekanal, der von Javascript-Programmen genutzt werden kann. Im Gegensatz zur Standardausgabe, die für normale Ausgaben wie Konsolenausgaben verwendet wird, wird die Standardfehlerausgabe für die Ausgabe von Fehlern, Warnungen oder anderen wichtigen Informationen verwendet. Es kann auch verwendet werden, um Debugging-Informationen während der Entwicklung zu drucken.

```Javascript
console.error("Oops, something went wrong!");//Ausgabe: Oops, something went wrong!
```

Tiefer eintauchen
Der ursprüngliche Zweck hinter der Standardfehlerausgabe liegt in der Unix-Philosophie des "Alles ist eine Datei"-Konzepts. In Unix-Systemen gibt es verschiedene "Dateien" für verschiedene Aufgaben, und das Schreiben von Fehlern auf die Standardfehlerausgabe war Teil dieser Philosophie. Alternativ können Programme auch eine spezielle Methode namens "console.warn" verwenden, um Warnungen auszugeben, die normalerweise weniger schwerwiegend sind als Fehler.

Die Implementierung von Standardfehler in Javascript erfolgt mithilfe von "process.stderr", einem Objekt, das den Standardfehlerausgabestrom darstellt. Dies ermöglicht es Entwicklern, auf diesen Strom zuzugreifen und ihn für ihre Diagnosezwecke zu nutzen.

Weiterführende Links
Für weitere Informationen und Beispiele zur Verwendung von Standardfehler in der Praxis, schauen Sie sich die offizielle Dokumentation von Node.js an: https://nodejs.org/api/process.html#process_process_stderr