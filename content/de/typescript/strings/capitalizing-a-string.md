---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:40.189136-07:00
description: "Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben eines\
  \ gegebenen Strings in Gro\xDFbuchstaben umzuwandeln, falls er in Kleinbuchstaben\u2026"
lastmod: '2024-03-13T22:44:53.615169-06:00'
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben eines gegebenen\
  \ Strings in Gro\xDFbuchstaben umzuwandeln, falls er in Kleinbuchstaben vorliegt,\
  \ und oft den Rest des Strings unver\xE4ndert zu lassen."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
TypeScript, als eine Obermenge von JavaScript, ermöglicht verschiedene Methoden zum Kapitalisieren von Strings, die von reinen JavaScript-Ansätzen bis zur Nutzung von Drittanbieterbibliotheken für komplexere oder spezifische Anwendungsfälle reichen.

**Reiner JavaScript-Ansatz:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Beispiel-Ausgabe:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Diese Methode ist unkompliziert und stützt sich auf die `charAt()`-Methode, um auf den ersten Buchstaben des Strings zuzugreifen, und `toUpperCase()`, um ihn in einen Großbuchstaben umzuwandeln. Die `slice(1)`-Methode holt dann den Rest des Strings ab, lässt ihn unverändert.

**Mit der Lodash-Bibliothek:**

Für Projekte, die bereits die [Lodash](https://lodash.com/)-Bibliothek verwenden, können Sie dessen `_.capitalize`-Funktion nutzen, um dasselbe Ergebnis mit weniger Boilerplate-Code zu erzielen.

Zuerst installieren Sie Lodash:

```bash
npm install lodash
```

Dann nutzen Sie es in Ihrer TypeScript-Datei:

```typescript
import * as _ from 'lodash';

// Beispiel-Ausgabe:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Hinweis: Lodashs `_.capitalize`-Methode wandelt den Rest des Strings in Kleinbuchstaben um, was nicht immer gewünscht sein könnte.

**Mit einem Regulären Ausdruck:**

Ein regulärer Ausdruck kann eine prägnante Methode bieten, um den ersten Buchstaben eines Strings zu kapitalisieren, besonders wenn Sie den ersten Buchstaben jedes Wortes in einem String kapitalisieren müssen.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Beispiel-Ausgabe:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Diese Methode verwendet die `replace()`-Funktion, um nach jeder Wortgrenze gefolgt von einem alphanumerischen Zeichen (`\b\w`) zu suchen, und kapitalisiert jedes gefundene Zeichen. Sie ist besonders praktisch für Titel oder Überschriften.
