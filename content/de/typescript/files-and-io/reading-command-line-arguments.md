---
date: 2024-01-20 17:57:09.140786-07:00
description: "So geht's: In TypeScript k\xF6nnt ihr Kommandozeilenargumente mit `process.argv`\
  \ auslesen, das ein Array der \xFCbergebenen Argumente ist."
lastmod: '2024-03-13T22:44:53.645905-06:00'
model: gpt-4-1106-preview
summary: "In TypeScript k\xF6nnt ihr Kommandozeilenargumente mit `process.argv` auslesen,\
  \ das ein Array der \xFCbergebenen Argumente ist."
title: Lesen von Kommandozeilenargumenten
weight: 23
---

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
