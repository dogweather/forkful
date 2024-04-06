---
date: 2024-01-20 18:00:34.997069-07:00
description: "How to: Vor Jahrzehnten starteten HTTP-Anfragen meist nur beim Laden\
  \ einer Seite. AJAX \xE4nderte das: dynamische Anfragen ohne Neuladen. Heute gibt's\
  \ viele\u2026"
lastmod: '2024-04-05T22:51:08.201808-06:00'
model: gpt-4-1106-preview
summary: Vor Jahrzehnten starteten HTTP-Anfragen meist nur beim Laden einer Seite.
title: Einen HTTP-Request senden
weight: 44
---

## How to:
TypeScript mit Axios:

```typescript
import axios from 'axios';

async function fetchData() {
  try {
    const response = await axios.get('https://api.meineseite.de/daten');
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
}

fetchData();
```

Ausgabe:

```json
{
  "users": [
    { "id": 1, "name": "Max Mustermann" },
    { "id": 2, "name": "Erika Musterfrau" }
  ]
}
```

Zum Einrichten von Axios:

1. `npm install axios`
2. Obigen Code in einer `.ts` Datei.

TypeScript mit fetch API:

```typescript
async function fetchData() {
  try {
    const response = await fetch('https://api.meineseite.de/daten');
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

fetchData();
```

## Deep Dive
Vor Jahrzehnten starteten HTTP-Anfragen meist nur beim Laden einer Seite. AJAX änderte das: dynamische Anfragen ohne Neuladen. Heute gibt's viele Optionen: `XMLHttpRequest`, `fetch`, diverse Bibliotheken wie `jQuery.ajax`, `axios` oder `superagent`. Bei TypeScript achte man auf Typsicherheit – `axios` und `fetch` können über Generics die Datenstruktur definieren.

## See Also
- MDN zu `fetch`: https://developer.mozilla.org/de/docs/Web/API/Fetch_API
- Axios GitHub Repository: https://github.com/axios/axios
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
