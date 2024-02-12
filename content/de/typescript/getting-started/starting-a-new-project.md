---
title:                "Einen neuen Projekt starten"
aliases: - /de/typescript/starting-a-new-project.md
date:                  2024-01-20T18:04:43.412537-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt zu beginnen bedeutet, eine frische Codebasis anzulegen, um eine Idee oder Lösung umzusetzen. Programmierer starten neue Projekte, um kreative Lösungen zu entwickeln, Probleme zu lösen oder neue Werkzeuge und Bibliotheken auszuprobieren.

## How to:
Um ein neues TypeScript-Projekt zu starten, installiere zunächst Node.js und npm. Dann führe die folgenden Schritte aus:

```bash
# TypeScript-Compiler global installieren
npm install -g typescript

# Verzeichnis für das neue Projekt erstellen
mkdir mein-neues-projekt
cd mein-neues-projekt

# npm-Projekt initialisieren und Abhängigkeiten installieren
npm init -y
npm install typescript --save-dev

# tsconfig.json für TypeScript-Optionen erstellen
tsc --init
```

Jetzt kannst du eine neue `.ts`-Datei erstellen:

```TypeScript
// greeter.ts
function greet(name: string): string {
  return `Hallo, ${name}!`;
}

console.log(greet('Welt'));
```

Kompiliere die TypeScript-Datei in JavaScript:

```bash
tsc greeter.ts
```

Ausführen der generierten JavaScript-Datei:

```bash
node greeter.js
```

Sample output:

```
Hallo, Welt!
```

## Deep Dive

Bis TypeScript zuerst 2012 veröffentlicht wurde, mussten JavaScript-Entwickler ohne statische Typen arbeiten, was zu häufigen Laufzeitfehlern führte. TypeScript wurde als Ergänzung zu JavaScript eingeführt, um große und komplexe Projekte sicherer und wartbarer zu machen.

Alternativen zu TypeScript sind etwa Flow von Facebook oder der Einsatz von reinem JavaScript mit JSDoc für Typinformationen. TypeScript selbst kann jedoch direkt in JavaScript umgewandelt werden, unterstützt die neuesten ECMA-Features und hat eine große Community sowie umfangreiche Typdefinitionen.

Das Herzstück der TypeScript-Konfiguration ist die `tsconfig.json`-Datei. Hier kannst du Compileroptionen wie `target` für die ECMAScript-Zielversion oder `strict` für strenge Typüberprüfungen einstellen. Mit der korrekten Konfiguration ist das Aufsetzen neuer Projekte schnell und fehlerresistent.

## See Also

- Offizielle TypeScript-Dokumentation: [TypeScript Docs](https://www.typescriptlang.org/docs/)
- TypeScript GitHub Repository: [Microsoft/TypeScript on GitHub](https://github.com/Microsoft/TypeScript)
- TypeScript Deep Dive Buch: [TypeScript Deep Dive](https://basarat.gitbooks.io/typescript/)
- Node.js und npm Installationsanleitung: [Node.js](https://nodejs.org/)
