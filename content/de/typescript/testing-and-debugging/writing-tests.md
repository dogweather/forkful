---
title:                "Tests Schreiben"
aliases:
- /de/typescript/writing-tests.md
date:                  2024-02-03T19:32:08.777954-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schreiben"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben von Tests in TypeScript umfasst das Erstellen automatisierter Skripte, um die Funktionalität und Richtigkeit Ihres Codes zu überprüfen. Programmierer machen das, um Zuverlässigkeit zu gewährleisten, schnell Fehler zu finden und ein wartbares Code-Wachstum zu erleichtern, da die statische Typisierung von TypeScript eine Ebene der Vorhersehbarkeit beim JavaScript-Testing hinzufügt.

## Wie geht das:
TypeScript funktioniert harmonisch mit den meisten JavaScript-Testframeworks. Zum Demonstrationszweck verwenden wir Jest, ein beliebtes Testframework, aufgrund seiner Konfigurationsfreien Einrichtung für TypeScript-Projekte.

Stellen Sie zunächst sicher, dass Sie Jest und die notwendigen TypeScript-Typen installiert haben:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Richten Sie anschließend Jest so ein, dass es mit TypeScript funktioniert, indem Sie die `jest.config.js` ändern oder eine neue erstellen:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Schreiben wir nun eine einfache Funktion und einen Test dafür. Betrachten Sie eine `sum.ts`-Datei mit der folgenden Funktion:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Erstellen Sie eine Testdatei namens `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('addiert 1 + 2 zu 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Führen Sie Ihre Tests mit folgendem Befehl aus:

```bash
npx jest
```

Die Ausgabe, die einen bestandenen Test anzeigt, sollte etwa so aussehen:

```plaintext
 PASS  ./sum.test.ts
  ✓ addiert 1 + 2 zu 3 (2 ms)
```

Für asynchronen Code bietet Jest Unterstützung mit `async/await`. Angenommen, Sie haben eine asynchrone `fetchData`-Funktion:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Ihr Test unter Verwendung asynchroner Funktionen:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('holt Daten erfolgreich', async () => {
  expect(await fetchData()).toBe('data');
});
```

Beim Ausführen Ihrer Tests wird Jest auf die Erfüllung des Versprechens warten und asynchrone Operationen korrekt testen.

Denken Sie daran, effektives Testen beinhaltet das Schreiben mehrerer Tests für verschiedene Szenarien, einschließlich Grenzfälle, um sicherzustellen, dass Ihr TypeScript-Code wie erwartet funktioniert.
