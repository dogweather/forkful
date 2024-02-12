---
title:                "Fehlerbehandlung"
aliases: - /de/typescript/handling-errors.md
date:                  2024-01-26T00:58:10.059014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung bedeutet, mit dem Unerwarteten zu rechnen; es geht darum, wie wir damit umgehen, wenn in unserem Code etwas schiefgeht. Wir tun dies, um Abstürze zu vermeiden und den Benutzern ein reibungsloses Erlebnis zu bieten, auch wenn das Unerwartete eintritt.

## Wie geht das:
In TypeScript umfasst die Fehlerbehandlung oft `try`, `catch`- und `finally`-Blöcke.

```typescript
function riskyOperation() {
  throw new Error("Etwas ist schiefgelaufen!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Fehler gefangen:", error.message);
  } finally {
    console.log("Das wird immer ausgeführt, Fehler oder nicht.");
  }
}

handleErrors();
```

Beispielausgabe:

```
Fehler gefangen: Etwas ist schiefgelaufen!
Das wird immer ausgeführt, Fehler oder nicht.
```

Asynchrones Beispiel mit Promises:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simuliere einen Fehler
    reject("Kläglich gescheitert");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Asynchronen Fehler gefangen:", error);
  }
}

handleAsyncErrors();
```

Beispielausgabe:

```
Asynchronen Fehler gefangen: Kläglich gescheitert
```

## Vertiefung
Fehlerbehandlung ist seit Beginn der Programmierung ein Eckpfeiler. In TypeScript, das auf JavaScript aufbaut, wurde die Fehlerbehandlung mit der Einführung von async/await in ECMAScript 2017 robuster. Vorher verließen wir uns oft auf Callback-Funktionen und Promises, um Fehler in asynchronem Code zu behandeln.

Eine Alternative zu `try/catch` in TypeScript ist die Verwendung von Fehlergrenzen, die von Frameworks wie React bereitgestellt werden. Für die serverseitige Behandlung können wir in Plattformen wie Express.js Middleware verwenden, um das Fehlermanagement zu zentralisieren.

Implementierungstechnisch hat TypeScript keinen eigenen Fehlerbehandlungsmechanismus, sondern verlässt sich auf JavaScripts Mechanismus. Benutzerdefinierte Fehlerklassen können die `Error`-Klasse erweitern, um detailliertere Fehlerinformationen anzubieten.

## Siehe auch
- [MDN über try/catch](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await auf MDN](https://developer.mozilla.org/de/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Verwendung von Fehlergrenzen in React](https://de.reactjs.org/docs/error-boundaries.html)
- [Fehlerbehandlung in Express.js](https://expressjs.com/de/guide/error-handling.html)
