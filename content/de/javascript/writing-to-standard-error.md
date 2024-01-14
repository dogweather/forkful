---
title:                "Javascript: Schreiben auf den Standardfehler"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Schreiben von Standardfehlern oder Fehlerprotokollen ist ein wichtiger Aspekt der Javascript Programmierung, der oft übersehen wird. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, warum es wichtig ist, auf Standardfehler zu schreiben und wie man es richtig macht.

## Wie man auf Standardfehler schreibt

Die Verwendung von `console.log()` ist eine gängige Methode, um Nachrichten in der Konsole auszugeben. Aber was ist, wenn wir spezifische Fehlermeldungen ausgeben möchten? Dafür gibt es `console.error()`, das uns ermöglicht, Nachrichten als Fehler auszugeben und sie in einem separaten Fehlerprotokoll aufzuzeichnen.

Beispielcode:

```Javascript
try {
  // Versuche den Code auszuführen
  let result = dividieren(10, 0);
  console.log("Das Ergebnis ist: " + result);
} catch (err) {
  console.error("Es ist ein Fehler aufgetreten: " + err);
}
```

Ergebnis in der Konsole:

```
Es ist ein Fehler aufgetreten: Error: Versuch, durch Null zu teilen
```

Wie man sieht, können wir mithilfe von `console.error()` nicht nur Fehlermeldungen ausgeben, sondern auch die Ursache des Fehlers, in diesem Fall durch Null zu teilen.

Ein weiteres nützliches Feature ist die Verwendung von `console.trace()`. Mit dieser Methode können wir den ursprünglichen Aufrufstapel der Funktion anzeigen, was uns bei der Fehlersuche in komplexen Programmen helfen kann.

Beispielcode:

```Javascript
function doSomething() {
  doSomethingElse();
}

function doSomethingElse() {
  let result = dividieren(10, 0);
  console.log("Das Ergebnis ist: " + result);
}

try {
  doSomething();
} catch (err) {
  console.error("Es ist ein Fehler aufgetreten: " + err);
  console.trace();
}
```

Ergebnis in der Konsole:

```
Es ist ein Fehler aufgetreten: Error: Versuch, durch Null zu teilen
Trace: doSomethingElse()
    at doSomething (script.js:8)
    at script.js:13
```

Wie man sieht, können wir mithilfe von `console.trace()` den kompletten Aufrufstapel des Fehlers verfolgen und so besser verstehen, wo der Fehler aufgetreten ist.

## Tieferer Einblick

Das Schreiben auf Standardfehler ist besonders hilfreich bei der Fehlersuche und -behebung in komplexen Programmen. Mit den verschiedenen Methoden von `console` können wir detaillierte Informationen über Fehler ausgeben und so schneller herausfinden, wo das Problem liegt.

Es ist auch möglich, den Fehlerprotokollausgabestrom zu ändern, indem man den Standardfehler-Stream `STDERR` auf einen anderen Stream umleitet. So können wir beispielsweise Fehler in eine externe Datei schreiben, anstatt sie in der Konsole auszugeben.

## Siehe auch

- https://nodejs.org/api/console.html
- https://developer.mozilla.org/de/docs/Web/API/Console
- https://www.w3schools.com/js/js_errors.asp