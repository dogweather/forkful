---
date: 2024-01-20 17:55:23.032718-07:00
description: "How to: Historisch gesehen ist das Lesen von Dateien so alt wie die\
  \ Programmierung selbst. In TypeScript verwenden wir meist das 'fs' (File System)\
  \ Modul\u2026"
lastmod: '2024-04-05T21:53:55.526326-06:00'
model: gpt-4-1106-preview
summary: Historisch gesehen ist das Lesen von Dateien so alt wie die Programmierung
  selbst.
title: Textdatei einlesen
weight: 22
---

## How to:
```TypeScript
// Einfaches Lesen einer Textdatei mit Node.js FS-Modul
import { readFileSync, promises as fsPromises } from 'fs';

// Synchron:
try {
  const data = readFileSync('example.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error(err);
}

// Asynchron:
fsPromises.readFile('example.txt', 'utf8')
  .then(data => console.log(data))
  .catch(error => console.error(error));
```
Ausgabe:
```
// Inhalt der example.txt Datei wird ausgegeben
```

## Deep Dive
Historisch gesehen ist das Lesen von Dateien so alt wie die Programmierung selbst. In TypeScript verwenden wir meist das 'fs' (File System) Modul von Node.js, das es in synchronen und asynchronen Geschmacksrichtungen gibt. Alternativen zum FS-Modul könnten Streams für große Dateien oder Bibliotheken wie 'fs-extra' für zusätzliche Funktionalitäten sein. Bei der synchronen Methode blockiert der Prozess, bis die Datei gelesen ist – gut für schnelle Jobs. Asynchrones Lesen ist nicht blockierend und erfolgt über Promises oder async/await, besser geeignet für Performance-sensible Anwendungen.

## See Also
- Node.js FS Dokumentation: https://nodejs.org/api/fs.html
- fs-extra Modul: https://github.com/jprichardson/node-fs-extra
- Node.js Stream Handbuch: https://nodejs.org/api/stream.html
