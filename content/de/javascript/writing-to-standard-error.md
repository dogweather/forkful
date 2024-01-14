---
title:                "Javascript: Schreiben in die Standardfehlerausgabe"
simple_title:         "Schreiben in die Standardfehlerausgabe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben zu Standardfehlern (engl: writing to standard error) in der Programmierung kann ein entscheidendes Werkzeug sein, um Fehler und Probleme in unserem Code zu identifizieren und zu beheben. Durch die Ausgabe von Fehlermeldungen auf dem Standardfehlerausgabestrom (engl: standard error output stream) können wir genauere Informationen über die auftretenden Probleme erhalten und so unser Programm verbessern.

## Wie man das macht

In Javascript können wir mithilfe der Methode `console.error()` Fehlermeldungen auf dem Standardfehlerausgabestrom ausgeben. Dies ist besonders nützlich, wenn wir versuchen, Bugs in unserem Code zu finden oder wenn wir unsere Programme für die Produktion optimieren.

Ein einfaches Beispiel dafür wäre:

```Javascript
let num = "leer";

if(num === "leer") {
  console.error("Die Variable ist leer!");
}
```

Die Ausgabe dieses Codes würde folgendermaßen aussehen:

```
Die Variable ist leer!
```

Auf diese Weise können wir schnell und effektiv Fehler in unserem Code erkennen und beheben.

## Tiefer Tauchen

Manchmal kann es notwendig sein, zusätzliche Informationen über einen Fehler zu erhalten, wie zum Beispiel die Datei, in der der Fehler aufgetreten ist, oder die genaue Zeilennummer. Hierfür können wir die Funktion `console.trace()` verwenden, um einen Stack-Trace des Programms auszugeben. Dies kann uns helfen, den Fehler genauer zu lokalisieren und zu verstehen.

Die Verwendung von `console.trace()` könnte beispielsweise so aussehen:

```Javascript
function divide(a, b) {
  if(b === 0) {
    console.error("Division durch 0 ist nicht erlaubt!");
    console.trace();
  } else {
    return a / b;
  }
}

divide(10, 0);
```

Die Ausgabe würde dann folgendermaßen aussehen:

```
Division durch 0 ist nicht erlaubt!
Trace
    at divide (<anonymous>:4:13)
    at <anonymous>:11:1
    at <anonymous>:12:3
```

Durch das Erstellen eines Stack-Traces können wir den Fehler zurückverfolgen und so effizienter beheben.

## Siehe auch

- [MDN Web Docs - Fehlerbehandlung und Fehlererkennung in Javascript](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Errors)
- [Techtutorials - Debugging mit der Konsole in Javascript](https://www.techtutorials.de/javascript-tutorial/javascript-debugging-mit-dem-console.html)
- [W3Schools - console.error()](https://www.w3schools.com/jsref/met_console_error.asp)