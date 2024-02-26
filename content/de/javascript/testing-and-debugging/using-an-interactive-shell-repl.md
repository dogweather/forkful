---
date: 2024-01-26 04:15:28.838163-07:00
description: "Interaktive Shells oder REPLs (Read-Eval-Print Schleifen) erm\xF6glichen\
  \ es Ihnen, Code spontan auszuf\xFChren, Funktionen, Algorithmen zu testen oder\
  \ mit Ideen\u2026"
lastmod: '2024-02-25T18:49:51.318323-07:00'
model: gpt-4-0125-preview
summary: "Interaktive Shells oder REPLs (Read-Eval-Print Schleifen) erm\xF6glichen\
  \ es Ihnen, Code spontan auszuf\xFChren, Funktionen, Algorithmen zu testen oder\
  \ mit Ideen\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Interaktive Shells oder REPLs (Read-Eval-Print Schleifen) ermöglichen es Ihnen, Code spontan auszuführen, Funktionen, Algorithmen zu testen oder mit Ideen herumzuspielen. Sie sind die Notizblöcke des Codierens, schnell und unkompliziert, ohne eine vollständige Entwicklungs­umgebung einrichten zu müssen.

## Wie:
Node.js wird mit einer über das Terminal zugänglichen REPL geliefert. Öffnen Sie es, und Sie können sofort loslegen. Hier ein Vorgeschmack:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Einfach, richtig? Variablen definieren, Funktionen ausführen oder Schleifen laufen lassen. Wenn Sie fertig sind, bringt Sie `.exit` zurück in die reale Welt.

## Tiefergehende Betrachtung
REPLs gibt es seit den 1960er Jahren – LISP hat das Konzept eingeführt. Die Idee: dem Programmierer sofortiges Feedback geben. Alternativen? Neben der Node.js-REPL gibt es browserbasierte Konsolen wie die Chrome DevTools, Online-Sandkästen wie JSFiddle oder vollständige IDEs wie VSCode mit interaktiven Spielwiesen.

Unter der Haube folgen REPL-Abläufe typischerweise: 
1. Eingabe lesen
2. Code kompilieren und ausführen
3. Ausgabe drucken
4. Zurück zur Schleife

Es ist ein einfacher, doch effektiver Zyklus, der das interaktive Codieren massiv beeinflusst hat.

## Siehe auch
- [Node.js REPL Dokumentation](https://nodejs.org/api/repl.html)
- [Einführung in JavaScript-Module auf REPLs von Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
