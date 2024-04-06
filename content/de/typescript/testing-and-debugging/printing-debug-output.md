---
date: 2024-01-20 17:53:37.182179-07:00
description: 'How to: (Wie?) Sample Output.'
lastmod: '2024-04-05T21:53:55.510414-06:00'
model: gpt-4-1106-preview
summary: (Wie?) Sample Output.
title: Debug-Ausgaben drucken
weight: 33
---

## How to: (Wie?)
```typescript
console.log('Hello Debug!');

let variable = 42;
console.debug('Der Wert der Variable ist:', variable);

// Gruppierung von Ausgaben
console.group('Meine Gruppe');
console.log('Innerhalb der Gruppe');
console.groupEnd();
```
Sample Output:
```
Hello Debug!
Der Wert der Variable ist: 42
Meine Gruppe
  Innerhalb der Gruppe
```

## Deep Dive (Tieftauchgang)
Früher schrieben wir Ausgaben in Log-Dateien. Heute verwenden wir oft die Konsole. Mit `console.log` bekommst du einfache Nachrichten. `console.debug` ist spezifischer fürs Debugging gedacht, oft ignoriert in Produktionsumgebungen. `console.group` hilft, Nachrichten zu organisieren und die Lesbarkeit zu verbessern. Es gibt noch `console.info`, `console.warn` und `console.error` für differenzierte Ausgaben. Mit modernen Entwicklertools wie Chrome DevTools kannst du sogar nach Nachrichtentyp filtern.

## See Also (Siehe Auch)
- [MDN Web Docs - Console](https://developer.mozilla.org/de/docs/Web/API/Console)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Node.js Console Class](https://nodejs.org/api/console.html)
- [Chrome DevTools](https://developer.chrome.com/docs/devtools/)
