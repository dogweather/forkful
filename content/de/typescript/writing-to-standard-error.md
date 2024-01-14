---
title:                "TypeScript: Schreiben in die Standardfehlerausgabe"
simple_title:         "Schreiben in die Standardfehlerausgabe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

In vielen Programmiersprachen gibt es die Möglichkeit, Fehler und andere Informationen auf der Konsole auszugeben. Dies kann für Entwickler:innen sehr nützlich sein, um Fehler zu finden und zu beheben. Doch warum sollte man speziell in TypeScript auf die Standardfehlerausgabe zurückgreifen?

Die Antwort liegt in der Typensicherheit von TypeScript. Da TypeScript statisch typisiert ist, können viele Fehler bereits während des Schreibens des Codes erkannt werden. Jedoch gibt es auch Fälle, in denen der Code trotz Typenüberprüfung eine Fehlermeldung ausgibt. In diesen Fällen kann das Schreiben auf die Standardfehlerausgabe helfen, um die Ursache des Fehlers zu finden.

## Wie geht das?

Um in TypeScript auf die Standardfehlerausgabe zu schreiben, gibt es die Funktion `console.error()`. Diese kann mit einer beliebigen Nachricht als Parameter aufgerufen werden. Hier ein Beispiel:

```TypeScript
const name = "Max";
console.error("Hello " + name + ", there is a problem with your code.")
```

Das obige Beispiel würde folgende Ausgabe erzeugen:

```
Hello Max, there is a problem with your code.
```

Wenn der Code jedoch ausführlich ist und das Finden des Fehlers erschwert, kann es hilfreich sein, zusätzliche Informationen in die Meldung einzufügen. Dafür bietet TypeScript die Template-Strings-Syntax an, bei der Variablen direkt in einen String eingefügt werden können. Hier ein Beispiel:

```TypeScript
const name = "Max";
const lineNumber = 10;
console.error(`Hello ${name}, there is a problem with your code on line ${lineNumber}.`)
```

Die Ausgabe sieht dann so aus:

```
Hello Max, there is a problem with your code on line 10.
```

## Deep Dive

Wenn man tiefer in das Thema eintaucht, wird man feststellen, dass die Standardfehlerausgabe in TypeScript noch weitaus mehr Möglichkeiten bietet. Mit den sogenannten Decorators können bestimmte Funktionen oder Klassen direkt mit einer Error-Handler-Funktion versehen werden. Diese wird bei einem auftretenden Fehler automatisch aufgerufen und kann so zur Fehlerbehandlung genutzt werden. Eine ausführliche Anleitung dazu kann man in der [offiziellen TypeScript Dokumentation](https://www.typescriptlang.org/docs/handbook/decorators.html#adding-an-error-handler) finden.

## Siehe auch

- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs)
- [Beispiel für die Verwendung von `console.error()` in TypeScript](https://www.codegrepper.com/code-examples/typescript/typescript+console.error)
- [Einführung in Decorators in TypeScript](https://www.sitepoint.com/javascript-decorators-what-they-are/)