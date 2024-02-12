---
title:                "Lesen von Kommandozeilenargumenten"
aliases:
- /de/javascript/reading-command-line-arguments/
date:                  2024-01-20T17:56:11.145890-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Kommandozeilenargumente sind die Parameter, die du deinem Programm beim Start übergibst. Programmierer nutzen sie, um ihre Anwendungen flexibel zu steuern, ohne den Code jedes Mal anpassen zu müssen.

## So geht’s:

```javascript
// myscript.js
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// In der Kommandozeile
$ node myscript.js user123 passw0rd

// Ausgabe:
// 0: Pfad/zu/deinem/node.exe
// 1: Pfad/zu/deinem/myscript.js
// 2: user123
// 3: passw0rd
```

Nutze `process.argv` für den einfachen Zugriff. Aber Achtung: Die ersten beiden Argumente sind der Node-Befehl und der Skriptpfad.

## Tiefere Einblicke

In alten Zeiten, als Node.js noch jung war, war `process.argv` typischerweise die erste Wahl zum Lesen von Kommandozeilenargumenten. Heute gibt's Libraries wie `yargs` oder `commander`, die mächtiger sind und mehr Komfort bieten. Beispielsweise erlauben sie die Definition von Optionen, Schaltern und sogar die Validierung von Eingaben.

### Implementierungsdetails
`process.argv` ist ein Array, das alle Kommandozeilenargumente enthält. Du kannst es direkt verwenden oder mit anderen Modulen kombinieren, die darauf aufbauen.

### Alternativen
- `yargs` hilft bei der Argumenten-Parsing, bietet Hilfe-Texte und viel mehr.
- `commander` ist ähnlich, aber etwas leichtgewichtiger und einfacher.

### Historischer Kontext
Es ist eine lang gehegte Tradition in der Programmierung, Argumente über die Kommandozeile zu übergeben. Dies stammt aus den Tagen der Terminal- und Shell-Skripte. JavaScript auf dem Server (mittels Node.js) führte diese Möglichkeit weiter.

## Siehe Auch

- Node.js Dokumentation zu `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- `yargs` GitHub Repository: https://github.com/yargs/yargs
- `commander` GitHub Repository: https://github.com/tj/commander.js
