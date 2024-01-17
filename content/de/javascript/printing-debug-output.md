---
title:                "Debugoutput ausgeben"
html_title:           "Javascript: Debugoutput ausgeben"
simple_title:         "Debugoutput ausgeben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Programmieren werden oft zusätzliche Ausgaben, auch Debug-Ausgaben genannt, verwendet, um zu überprüfen, ob der Code wie erwartet funktioniert. Dies kann hilfreich sein, um Fehler zu finden und zu beheben, oder um den Programmablauf zu verstehen.

## Wie geht's?

Um Debug-Ausgaben in Javascript zu machen, kann die Funktion ```console.log()``` verwendet werden. Diese Funktion nimmt beliebig viele Argumente an und gibt diese auf der Konsole aus. Hier ist ein Beispiel:

```javascript
let name = "Hans";
console.log("Hallo " + name + "!");
```

Dieser Code würde die Ausgabe "Hallo Hans!" auf der Konsole erzeugen. In diesem Beispiel wird der Wert der Variablen "name" in die Ausgabe mit einbezogen.

## Tiefere Einblicke

Das Konzept der Debug-Ausgaben entstand in den Anfängen der Programmiersprache BASIC, als es keine integrierten Debugging-Tools gab. Heutzutage gibt es auch andere Möglichkeiten, um Probleme im Code zu finden, wie beispielsweise das Verwenden von Breakpoints in Entwicklungsumgebungen. Allerdings können Debug-Ausgaben immer noch nützlich sein, insbesondere in komplexen Anwendungen.

Es ist wichtig zu beachten, dass Debug-Ausgaben in produktiven Anwendungen entfernt werden sollten, da sie die Leistung beeinträchtigen können. Es ist auch ratsam, sorgfältig zu planen, welche Informationen ausgegeben werden sollen, um die Lesbarkeit und Effizienz der Debug-Ausgaben zu verbessern.

## Siehe auch

Weitere Informationen zu Debug-Ausgaben in Javascript finden Sie in der offiziellen Dokumentation von [Javascript.com](https://javascript.com/debugging) und in dieser hilfreichen [Blog-Post](https://blog.js-console.io/consolelog-usage-in-javascript-and-why-it-is-still-relevant/).