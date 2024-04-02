---
date: 2024-01-26 04:18:21.503230-07:00
description: "Ein Read-Eval-Print-Loop (REPL) ist eine Programmierumgebung, die einzelne\
  \ Benutzereingaben annimmt, ausf\xFChrt und das Ergebnis an den Benutzer zur\xFC\
  ckgibt.\u2026"
lastmod: '2024-03-13T22:44:53.632763-06:00'
model: gpt-4-0125-preview
summary: "Ein Read-Eval-Print-Loop (REPL) ist eine Programmierumgebung, die einzelne\
  \ Benutzereingaben annimmt, ausf\xFChrt und das Ergebnis an den Benutzer zur\xFC\
  ckgibt.\u2026"
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Was & Warum?
Ein Read-Eval-Print-Loop (REPL) ist eine Programmierumgebung, die einzelne Benutzereingaben annimmt, ausführt und das Ergebnis an den Benutzer zurückgibt. Programmierer verwenden ein REPL, um schnell mit Code-Snippets zu experimentieren, Fehler zu suchen und neue Sprachfunktionen ohne den Overhead der Erstellung einer vollständigen Anwendung zu lernen.

## Wie:
TypeScript kommt nicht mit einem eigenen REPL. Lassen Sie uns `ts-node` verwenden, eine TypeScript-Ausführungsumgebung für Node.js, die ein REPL enthält.

Zuerst installieren Sie es global:
```bash
npm install -g ts-node
```

Starten Sie das REPL, indem Sie `ts-node` in Ihre Kommandozeile tippen:
```bash
ts-node
```

Hier ist ein schnelles Snippet zum Ausprobieren:
```TypeScript
> let message: string = 'Hallo, REPL!';
> console.log(message);
Hallo, REPL!
>
```
Um die Sitzung zu beenden, drücken Sie `Strg+D`.

## Tiefergehend
Historisch waren REPLs prominent in Sprachen wie Lisp, die eine dynamische Codeauswertung ermöglichten. Das Konzept hat sich seitdem verbreitet und ist ein Grundpfeiler für interaktives Codieren in vielen Sprachen geworden.

Für TypeScript ist `ts-node` nicht Ihre einzige Option. Alternativen umfassen die Verwendung des TypeScript Playground in einem Webbrowser oder die Nutzung anderer Node.js-basierter REPLs, die TypeScript mit geeigneten Plugins unterstützen.

In Bezug auf die Implementierung verwendet `ts-node` die TypeScript-Compiler-API, um Code on-the-fly zu transpilieren, bevor er von Node.js ausgeführt wird. Dies gibt Ihnen sofortiges Feedback und ist besonders nützlich, um die neuesten Funktionen von TypeScript ohne Einrichtungsaufwand auszuprobieren.

Eines zu bedenken – obwohl ein REPL großartig für schnelle Tests ist, ersetzt es nicht das Schreiben von traditionellem, testbarem und wartbarem Code. Es ist ein Werkzeug zum Lernen und Erforschen, kein Ersatz für ordnungsgemäße Entwicklungspraktiken.

## Siehe auch
- [Offizielle TypeScript-Website](https://www.typescriptlang.org/)
- [ts-node auf GitHub](https://github.com/TypeStrong/ts-node)
- [Node.js REPL-Dokumentation](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
