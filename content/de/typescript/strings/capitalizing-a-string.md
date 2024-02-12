---
title:                "Einen String großschreiben"
aliases:
- /de/typescript/capitalizing-a-string/
date:                  2024-02-03T19:06:40.189136-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings bedeutet, den ersten Buchstaben eines gegebenen Strings in Großbuchstaben umzuwandeln, falls er in Kleinbuchstaben vorliegt, und oft den Rest des Strings unverändert zu lassen. Diese Operation wird typischerweise verwendet, um sicherzustellen, dass Eigennamen oder der Anfang von Sätzen in der Textverarbeitung den Grammatikregeln entsprechen, wodurch die Ausgaben professionell und lesbar erscheinen.

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
