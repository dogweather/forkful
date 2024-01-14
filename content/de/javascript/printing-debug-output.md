---
title:                "Javascript: Fehlerausgabe drucken"
simple_title:         "Fehlerausgabe drucken"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
Bevor wir in die Details eintauchen, wollen wir uns erstmal überlegen, warum wir Debug-Ausgaben in unserem JavaScript-Code verwenden sollten. Diese Art von Ausgaben können uns dabei helfen, Fehler in unserem Code zu finden und zu beheben. Sie bieten uns Einblicke in den Ablauf unseres Programms und zeigen uns, wo es eventuell zu Fehlfunktionen kommt. Insbesondere bei komplexen Anwendungen können Debug-Ausgaben sehr nützlich sein, um Probleme schnell zu erkennen und zu beheben.

## Wie
Um Debug-Ausgaben in unserer JavaScript-Anwendung zu erzeugen, können wir die `console.log()`-Funktion verwenden. Diese Funktion ermöglicht es uns, beliebige Werte in der Konsole unseres Browsers auszugeben. Schauen wir uns beispielsweise folgendes Code-Beispiel an:

```javascript
let name = "Max";
let age = 30;
console.log(`Mein Name ist ${name} und ich bin ${age} Jahre alt.`);
```

Wenn wir diesen Code ausführen, werden wir in der Konsole unseres Browsers folgende Ausgabe sehen:

```
Mein Name ist Max und ich bin 30 Jahre alt.
```

Hier haben wir die Werte unserer Variablen `name` und `age` in der Ausgabe angezeigt, um sicherzustellen, dass die Werte korrekt zugewiesen wurden. Wir können `console.log()` auch verwenden, um den Wert von bestimmten Ausdrücken oder Variablen zu überprüfen, während unser Programm ausgeführt wird.

Wir können auch verschiedene Arten von Ausgaben erzeugen, indem wir verschiedene Methoden der Konsole verwenden, z.B. `console.error()` für Fehlermeldungen oder `console.warn()` für Warnungen. Es ist auch möglich, Objekte oder Arrays in der Konsole auszugeben und ihre Inhalte zu untersuchen.

## Deep Dive
Neben der `console.log()`-Funktion gibt es noch weitere Methoden und Eigenschaften, die uns bei der Fehlerbehebung und Analyse unseres Codes helfen können. Zum Beispiel können wir `console.table()` verwenden, um Daten in tabellarischer Form in der Konsole anzuzeigen, oder `console.group()` und `console.groupEnd()` verwenden, um zusammengehörige Ausgaben zusammenzufassen.

Darüber hinaus können wir auch die sogenannten "breakpoints" verwenden, um den Ablauf unseres Codes zu unterbrechen und uns die Werte von Variablen in einem bestimmten Bereich anzusehen. Diese Funktion ist besonders hilfreich beim Debuggen von komplexem Code, da sie es uns ermöglicht, den Zustand unseres Programms zu einem bestimmten Zeitpunkt zu untersuchen.

## Siehe auch
- [Einführung in das Debugging in JavaScript](https://developer.mozilla.org/de/docs/Web/JavaScript/Debugging)
- [Die verschiedenen Methoden der JavaScript-Konsole](https://developer.mozilla.org/de/docs/Web/API/Console#%C3%9Cbersicht_%C3%BCber_die_console-API_Intervallconsole-object)