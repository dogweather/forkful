---
title:    "Javascript: Schreiben auf Standardfehler"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens gibt es viele verschiedene Arten, wie wir unsere Ergebnisse ausgeben können. Eine davon ist, das Schreiben nach Standard Error. Aber warum sollte man das überhaupt tun?

Das Schreiben in den Standard Error ist wichtig, um Fehlermeldungen und andere wichtige Informationen anzuzeigen, die nicht Teil des normalen Programmausgangs sind. Diese Fehlermeldungen können dem Entwickler helfen, Schwachstellen in seinem Code zu identifizieren und zu beheben, um sicherzustellen, dass das Programm reibungslos funktioniert. Es ist auch hilfreich bei der Fehlersuche und Debugging.

## Wie es geht

Um Text in den Standard Error zu schreiben, können wir einfach die `console.error()` Funktion in Javascript verwenden. Hier ist ein Beispiel:

```Javascript
console.error("Es ist ein Fehler aufgetreten.");
```

Dieser Code wird den angegebenen Text in roter Schrift in der Konsole ausgeben, um zu zeigen, dass es sich um einen Fehler handelt. Hier ist ein Beispiel für die Ausgabe:

```
Es ist ein Fehler aufgetreten.
```

Diese Methode kann auch Variablen und andere Ausdrücke als Argumente akzeptieren. Hier ist ein weiteres Beispiel:

```Javascript
let name = "Max";

console.error("Der Name ist: " + name);
```

Dieser Code wird den Wert der Variablen `name` in der Konsole ausgeben:

```
Der Name ist: Max
```

Es ist wichtig zu beachten, dass das Schreiben in den Standard Error die Ausgabe nicht unterbricht oder das Programm zum Absturz bringt. Es ist einfach eine Möglichkeit, wichtige Informationen anzuzeigen, ohne die normale Programmausgabe zu beeinträchtigen.

## Tiefergehende Informationen

Beim Schreiben in den Standard Error gibt es ein paar Dinge zu beachten. Zunächst ist es wichtig zu wissen, dass es verschiedene Arten von Fehlermeldungen gibt, die spezifische Funktionen erfüllen. Einige davon sind:

- `console.warn()` - gibt eine Warnung aus, die darauf hinweist, dass etwas nicht ganz korrekt ist, aber das Programm trotzdem funktioniert.
- `console.assert()` - gibt eine Fehlermeldung aus, wenn eine Bedingung nicht erfüllt ist.
- `console.table()` - gibt ein Objekt oder Array als Tabelle formatiert aus.

Darüber hinaus kann das Schreiben in den Standard Error nützlich sein, wenn wir mit Ausnahmen und Fehlerbehandlung arbeiten. Wir können bestimmte Fehlermeldungen auslösen und an bestimmten Stellen im Code gezielt Ausgaben machen, um uns bei der Fehlersuche zu helfen.

Es ist auch wichtig zu beachten, dass das Schreiben in den Standard Error nicht der einzige Weg ist, um Fehlermeldungen anzuzeigen. Wir können auch `console.log()` verwenden, um Text in der Konsole auszugeben. Der Unterschied besteht darin, dass `console.error()` dafür ausgelegt ist, wichtige und kritische Informationen hervorzuheben, während `console.log()` für allgemeine Ausgaben verwendet werden kann.

## Siehe auch

- [JavaScript Fehlerbehandlung auf MDN](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Statements/try...catch)
- [So verwenden Sie die Konsole in Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/console/)
- [Grundlagen der Fehlerbehandlung in JavaScript](https://www.w3schools.com/js/js_errors.asp)