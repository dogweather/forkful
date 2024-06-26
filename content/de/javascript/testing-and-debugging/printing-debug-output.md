---
date: 2024-01-20 17:52:52.113471-07:00
description: 'So geht''s: Das Drucken von Debug-Infos kann einfach mit `console.log()`
  gemacht werden. Hier ein Beispiel.'
lastmod: '2024-03-13T22:44:54.268690-06:00'
model: gpt-4-1106-preview
summary: Das Drucken von Debug-Infos kann einfach mit `console.log()` gemacht werden.
title: Debug-Ausgaben drucken
weight: 33
---

## So geht's:
Das Drucken von Debug-Infos kann einfach mit `console.log()` gemacht werden. Hier ein Beispiel:

```Javascript
function add(a, b) {
  console.log("add wurde aufgerufen mit a:", a, "und b:", b);
  return a + b;
}

console.log("Ergebnis:", add(2, 3));
```

Ausgabe:
```
add wurde aufgerufen mit a: 2 und b: 3
Ergebnis: 5
```

Für komplexere Datenstrukturen nutzt man besser `console.table()` oder `console.dir()`.

```Javascript
const obj = { a: 1, b: { c: 2 } };

console.table(obj);
console.dir(obj);
```

## Hinter den Kulissen:
Früher haben Entwickler*innen oft `alert()` für Debugging genutzt, aber das ist nicht praktisch, weil es die Ausführung anhält. `console.log()` wurde mit der Zeit zum Standard, weil man damit Informationen flott im Konsolen-Tool der meisten Browser sehen kann. Es gibt auch mächtigere Alternativen wie Debugger, die in Entwicklertools eingebaut sind, wo man den Code schrittweise durchgehen und sich Werte anschauen kann.

Es gibt auch andere `console`-Methoden, die hilfreich sein können:
- `console.error()` für Fehlermeldungen,
- `console.warn()` für Warnungen,
- `console.info()` für Infos.

Manche Werkzeuge wie Node.js ermöglichen es, die Konsolenausgabe in einer Datei zu speichern, was nützlich für Langzeit-Logs ist.

## Siehe auch:
- MDN Web Docs über `console`: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Node.js Dokumentation über Konsolen-Klasse: https://nodejs.org/api/console.html
- Chrome DevTools Dokumentation: https://developer.chrome.com/docs/devtools/
