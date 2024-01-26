---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Standard Error (stderr) ist ein Ausgabekanal für Fehlermeldungen und Diagnosen. Programmierer nutzen stderr, um Fehler von normalen Ausgaben zu trennen, was bei der Fehlersuche und dem Logging hilft.

## Anleitung:
```TypeScript
// Ein einfaches Beispiel für das Schreiben auf stderr in TypeScript
console.error('Dies ist ein Fehler');

// Beispiel für die Verwendung von stderr mit einer bedingten Aussage
if (!process.env.SOME_ENV_VARIABLE) {
    console.error('Umweltvariable SOME_ENV_VARIABLE nicht gesetzt!');
}

// Du kannst auch Fehlerobjekte auf stderr ausgeben
const error = new Error('Ein ernstes Problem ist aufgetreten');
console.error(error);
```
Ausgabe:
```
Dies ist ein Fehler
Umweltvariable SOME_ENV_VARIABLE nicht gesetzt!
Error: Ein ernstes Problem ist aufgetreten
```

## Deep Dive:
Stderr ist ein Standard-Dateistrom, getrennt von Standard Output (stdout), und existiert seit den frühsten Tagen von Unix. Es ermöglicht, dass Fehlermeldungen auch dann sichtbar bleiben oder umgeleitet werden, wenn der gewöhnliche Output umgeleitet oder in eine Datei geschrieben wird. In Node.js wird `console.error()` genutzt, um auf stderr zu schreiben, und bietet eine einfache API, die sowohl Strings als auch komplexe Fehlerobjekte unterstützt. Diese Trennung ist besonders nützlich, wenn Skripte automatisiert und deren Ergebnisse analysiert werden.

## Siehe Auch:
- Node.js Dokumentation zu `console.error()`: https://nodejs.org/api/console.html#consoleerror
- Ein tieferer Einblick in Unix-Standard-Streams: https://en.wikipedia.org/wiki/Standard_streams
- Artikel über effektives Logging in TypeScript: https://www.tslang.cn/docs/handbook/decorators.html
