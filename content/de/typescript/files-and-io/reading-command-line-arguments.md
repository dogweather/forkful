---
title:                "Lesen von Kommandozeilenargumenten"
aliases:
- /de/typescript/reading-command-line-arguments/
date:                  2024-01-20T17:57:09.140786-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente sind die Parameter, die einem Programm übergeben werden, wenn es gestartet wird. Programmierer nutzen sie, um das Verhalten ihrer Anwendung dynamisch zu steuern, ohne den Code ändern zu müssen.

## So geht's:
In TypeScript könnt ihr Kommandozeilenargumente mit `process.argv` auslesen, das ein Array der übergebenen Argumente ist.

```typescript
// Beispiel: process_argv.ts
const args = process.argv.slice(2); // Die ersten zwei Elemente ignorieren (node und script path)

console.log(args); // Gibt die Argumente als Array aus

// Nutzungsbeispiel:
// $ node process_argv.ts arg1 arg2 arg3
// Ausgabe: [ 'arg1', 'arg2', 'arg3' ]
```

Die `slice(2)`-Methode wird verwendet, um die ersten beiden Standardargumente zu entfernen: den Pfad zu Node.js und den zum Skript.

```typescript
// Argumente mit bestimmter Option verarbeiten
const args = process.argv.slice(2);
const options = args.filter(arg => arg.startsWith("--"));
const optionValues = options.map(option => {
  const [key, value] = option.split("=");
  return { key: key.replace("--", ""), value };
});

console.log(optionValues);

// Nutzungsbeispiel:
// $ node process_argv.ts --name=Max --age=28
// Ausgabe: [ { key: 'name', value: 'Max' }, { key: 'age', value: '28' } ]
```

## Tiefgang:
Historisch gesehen stammt das Konzept der Kommandozeilenargumente aus den Anfangstagen der Computer, als interaktive Shells die Hauptmethode zur Interaktion mit Betriebssystemen waren. In JavaScript und TypeScript hat die `process`-Globale, ein Überbleibsel von Node.js, die Fähigkeit, auf die Systemumgebung zuzugreifen, was beinhaltet, Kommandozeilenargumente zu lesen.

Alternativen zu `process.argv` umfassen Bibliotheken wie `yargs` oder `commander`, die komplexere Parsing-Funktionalitäten bieten, wie z.B. Flags, Aliase und Standardwerte für Argumente.

Die Implementation ist straightforward: `process.argv` ist ein einfaches Array, weshalb keine zusätzliche Initialisierung oder Konfiguration erforderlich ist, um Argumente zu lesen.

## Siehe auch:
- [Node.js process.argv Dokumentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [`yargs` GitHub Repository](https://github.com/yargs/yargs)
- [`commander` GitHub Repository](https://github.com/tj/commander.js)
