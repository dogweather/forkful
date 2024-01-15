---
title:                "Ausgabe von Debug-Meldungen"
html_title:           "Javascript: Ausgabe von Debug-Meldungen"
simple_title:         "Ausgabe von Debug-Meldungen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wichtiger Teil der Entwicklung mit Javascript. Das Drucken von Debug-Ausgaben kann dabei helfen, die genaue Ursache eines Fehlers oder eines unerwarteten Verhaltens zu finden. Es ist hilfreich, um die Logik des Codes zu überprüfen und mögliche Schwachstellen schnell zu erkennen.

## Wie

Um Debug-Ausgaben in Javascript zu drucken, gibt es verschiedene Möglichkeiten. Die einfachste Methode ist die Verwendung der ```console.log()``` Funktion. Diese Funktion nimmt beliebig viele Argumente an und druckt sie in der Entwicklerkonsole des Browsers aus.

```Javascript
// Einfaches Beispiel
console.log("Start des Programms...");
console.log("Variable x = ", x);
```

Die Ausgabe des obigen Codes könnte wie folgt aussehen:

```
Start des Programms...
Variable x = 3 
```

Ein weiteres nützliches Tool ist der `debugger` Befehl. Das Einfügen dieses Befehls in den Code stoppt die Ausführung des Programms an dieser Stelle und ermöglicht es, Schritt für Schritt durch den Code zu gehen und Variablen- und Objektwerte zu überprüfen. Dies ist besonders hilfreich, wenn der Fehler an einer bestimmten Stelle im Code auftritt.

```Javascript
function calculate(a, b) {
  debugger;
  return (a + b);
}

console.log(calculate(5,2)); // Ausgabe: 7
```

Zusätzlich können auch Bedingungen angegeben werden, in denen der `debugger` gestoppt werden soll, um nur an bestimmten Punkten im Code Halt zu machen.

```Javascript
function calculate(a, b) {
  if (a > 10) {
    debugger;
    return "a ist größer als 10";
  }
  return (a + b);
}

console.log(calculate(15, 6)); // Ausgabe: "a ist größer als 10"
```

## Deep Dive

Neben `console.log()` und `debugger` gibt es auch weitere Methoden, um Debug-Ausgaben in Javascript zu drucken. Dazu gehört unter anderem die Verwendung von Entwicklerwerkzeugen wie Chrome DevTools oder der Firefox-Entwicklerkonsole.

Eine andere Möglichkeit ist die Verwendung von Online-Debugging-Tools wie JSFiddle oder CodePen. Diese erlauben es, Code in einem virtuellen Umfeld auszuführen und die Ausgaben direkt auf der Webseite zu sehen.

Außerdem gibt es Javascript-Bibliotheken und Frameworks, die speziell für Debugging und Error-Logging entwickelt wurden, wie z.B. Sentry oder log4js. Diese bieten erweiterte Funktionen wie das Speichern von Debug-Ausgaben in Dateien oder das Senden von Benachrichtigungen bei bestimmten Fehlern.

## Siehe Auch

- [Javascript Debugging Guide (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [Debugging in Chrome DevTools (Google Developers)](https://developers.google.com/web/tools/chrome-devtools/javascript)
- [Sentry Documentation](https://docs.sentry.io/)