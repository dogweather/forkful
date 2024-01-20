---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Drucken von Debug-Ausgaben in Javascript

## Was & Warum?
Drucken von Debug-Ausgaben ist ein Vorgang, bei dem Programmierer Ausgaben anzeigen lassen, um Programmfehler aufzuspüren. Es hilft, das Verhalten des Programms während der Ausführung zu verfolgen, was das Aufspüren und Reparieren von Fehlern vereinfacht.

## Wie geht das:
Verwenden Sie die `console.log()` Funktion, um Ausgaben in der Konsole anzuzeigen.

Beispiel:
```Javascript
const foo = "Hallo Welt";
console.log(foo);  //Ausgaben: Hallo Welt
```

Mit `console.log()` können auch komplexe Ausgaben gedruckt werden, wie zum Beispiel Arrays und Objekte.
```Javascript
const arr = [1, 2, 3];
console.log(arr);  //Ausgaben: [1, 2, 3]

const obj = { name: "Max", age: 30 };
console.log(obj);  //Ausgaben: { name: "Max", age: 30 }
```

## Tiefere Einblicke
Historisch gesehen wurden Debug-Ausgaben auf Registerkarten, Zettel oder auf Bildschirmen gedruckt. Im Kontext von Javascript ist `console.log()` zunächst in Firebug, einem Firefox's Debugger, und später in anderen Browsern eingeführt worden.

Es gibt Alternativen zur `console.log()` Funktion, z.B. `console.debug()`, `console.info()`, `console.warn()` und `console.error()`, die verschiedene Ausgabelevel darstellen und zur Strukturierung der Ausgaben hilfreich sein können.

Beachten Sie, dass die Nutzung der `console.log()` Funktion bei der Auslieferung einer App vermieden werden sollte, da sie die Performance beeinträchtigen kann und vertrauliche Informationen preisgeben kann.

## Siehe auch
- [MDN Doku - console.log()](https://developer.mozilla.org/de/docs/Web/API/Console/log)
- [Javascript Info - Debugging in Chrome](https://javascript.info/debugging-chrome)
- [W3Schools - Javascript Output](https://www.w3schools.com/js/js_output.asp)