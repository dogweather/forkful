---
title:                "Schreiben auf Standardfehler"
html_title:           "TypeScript: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man sich dazu entscheiden könnte, in das Standardfehlerausgabefenster zu schreiben. Einer der häufigsten Gründe ist, dass diese Methode dabei hilft, Fehler in der Codeausführung zu identifizieren und zu debuggen. Außerdem ist es eine effektive Möglichkeit, um Fehlermeldungen und Warnungen anzuzeigen, die dem Benutzer wichtige Informationen über die Funktionalität des Programms geben.

## Wie man es macht

Das Schreiben in das Standardfehlerausgabefenster kann in TypeScript auf zwei Arten erreicht werden.

Die erste Methode ist, die `console.error()` Funktion zu nutzen. Diese Funktion akzeptiert eine beliebige Anzahl von Argumenten und gibt diese als Fehlermeldung im Standardfehlerausgabefenster aus.

```TypeScript
console.error("Dies ist ein Beispiel für einen Fehler");
// Ausgabe: Dies ist ein Beispiel für einen Fehler
```

Die zweite Methode ist, die globale Variable `process.stderr` zu verwenden. Diese Variable ist ein Objekt, das den Standardfehlerausgabestrom darstellt. Um in diesen Strom zu schreiben, verwenden wir die `write()` Methode.

```TypeScript
process.stderr.write("Dies ist ein Beispiel für einen Fehler");
// Ausgabe: Dies ist ein Beispiel für einen Fehler
```

## Tiefergehende Informationen

Ein wichtiger Unterschied zwischen `console.error()` und `process.stderr.write()` ist ihre Ausgabemethode. Die `console.error()` Funktion gibt standardmäßig eine rote Fehlermeldung aus, während `process.stderr.write()` die Ausgabe im Standardformat ausgibt.

Eine weitere wichtige Tatsache ist, dass `console.error()` primär für den Einsatz in der Entwicklungsumgebung gedacht ist, während `process.stderr.write()` für Produktionssysteme besser geeignet ist. Das liegt daran, dass die `console` Funktionen für Entwicklungs- und Testzwecke oft von anderen Modulen überschrieben werden.

## Siehe auch

- [Node.js-Dokumentation zu `process.stderr.write()`](https://nodejs.org/api/process.html#process_process_stderr)
- [Node.js-Dokumentation zu `console.error()`](https://nodejs.org/api/console.html#console_console_error_data_args)