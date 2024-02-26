---
date: 2024-01-26 00:55:15.901709-07:00
description: "Fehlerbehandlung ist der Umgang mit Situationen, wenn im Code etwas\
  \ schiefgeht. Sie ist entscheidend, weil sie hilft, dass Programme anmutig scheitern\
  \ und\u2026"
lastmod: '2024-02-25T18:49:51.324224-07:00'
model: gpt-4-1106-preview
summary: "Fehlerbehandlung ist der Umgang mit Situationen, wenn im Code etwas schiefgeht.\
  \ Sie ist entscheidend, weil sie hilft, dass Programme anmutig scheitern und\u2026"
title: Fehlerbehandlung
---

{{< edit_this_page >}}

## Was & Warum?

Fehlerbehandlung ist der Umgang mit Situationen, wenn im Code etwas schiefgeht. Sie ist entscheidend, weil sie hilft, dass Programme anmutig scheitern und Benutzer klar anleiten, statt einfach abzustürzen und auszufallen.

## Wie geht das:

Hier ist der klassische `try-catch`-Block:

```javascript
try {
  // Code, der einen Fehler auslösen könnte
  let result = potentiallyRiskyOperation();
  console.log('Erfolg:', result);
} catch (error) {
  // Was tun, wenn ein Fehler ausgelöst wird
  console.error('Hoppla:', error.message);
}
```

Beispielausgabe, wenn kein Fehler auftritt:
```
Erfolg: 42
```

Und wenn ein Fehler auftritt:
```
Hoppla: Etwas ist schiefgelaufen
```

Für asynchronen Code, bei dem Promises beteiligt sind, verwenden Sie `try-catch` in einer `async`-Funktion:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Daten abgerufen:', data);
  } catch (error) {
    console.error('Fehler beim Abrufen der Daten:', error.message);
  }
}

fetchData();
```

## Tiefergehend

Die Fehlerbehandlung in JavaScript hat sich weiterentwickelt. In der Vergangenheit (ES3, circa 1999) hatten wir nur den `try-catch`-Block. Nicht super flexibel, aber er erledigte seinen Job.

ES6 (2015) führte Promises ein und gab uns `.then()` und `.catch()`, was es uns ermöglichte, asynchrone Fehler eleganter zu behandeln.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Daten abgerufen:', data))
  .catch(error => console.error('Fehler beim Abrufen der Daten:', error.message));
```

Was Implementierungsdetails angeht: Wenn ein Fehler ausgelöst wird, erstellen JavaScript-Engines ein `Error`-Objekt mit nützlichen Eigenschaften wie `message` und `stack`. Man kann auch benutzerdefinierte Fehlertypen erstellen, indem man die `Error`-Klasse erweitert – praktisch für komplexere Apps.

Alternativen? Man könnte die Fehlerbehandlung ignorieren (schlechte Idee), Callbacks mit fehlerersten Parametern verwenden (hallo, Node.js-Stil) oder es wird ausgefeilter mit Bibliotheken und Frameworks, die ihre Ansichten anbieten.

## Siehe auch

Für mehr Informationen zur Fehlerbehandlung:

- MDN zu try-catch: [MDN try...catch](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Statements/async_function)
- Ein Leitfaden zu Promises: [MDN Promises](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Erstellen und Auslösen benutzerdefinierter Fehler: [MDN Error](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/Error)
