---
date: 2024-01-20 17:44:09.768864-07:00
description: 'How to: Javascript bietet diverse Wege, um Webseiten herunterzuladen.
  Hier ein Beispiel mit `fetch`.'
lastmod: '2024-03-13T22:44:54.265158-06:00'
model: gpt-4-1106-preview
summary: Javascript bietet diverse Wege, um Webseiten herunterzuladen.
title: Webseite herunterladen
weight: 42
---

## How to:
Javascript bietet diverse Wege, um Webseiten herunterzuladen. Hier ein Beispiel mit `fetch`.

```Javascript
// Einfaches Beispiel, um eine Webseite herunterzuladen
fetch('https://example.com')
  .then(response => response.text())
  .then(data => {
    console.log(data); // Zeigt den HTML-Content der Seite
  })
  .catch(error => {
    console.error('Fehler beim Herunterladen der Seite:', error);
  });
```

`fetch` ist modern und verspricht-basiert. Ein einfacher `GET`-Request reicht oft.

## Deep Dive:
Das Herunterladen von Webseiten in JavaScript geschieht seit den Anfängen des Webs. Vor `fetch` gab es `XMLHttpRequest`, bekannt als XHR. XHR war kompliziert; `fetch` vereinfacht vieles.

Alternative Bibliotheken wie `axios` sind auch verfügbar. Sie bieten mehr Features, wie Interceptors und automatische JSON-Transformation.

Wichtig bei der Implementierung ist die Same-Origin-Policy, die im Browser Cross-Origin-Requests beschränkt. Um sie zu umgehen, braucht man CORS-Headers oder einen Proxy.

## See Also:
- MDN Web Docs zu `fetch`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Axios GitHub-Repository: https://github.com/axios/axios
- CORS-Erklärung auf MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS

Diese Links führen zu vertiefenden Informationen. Sie helfen, die Konzepte hinter dem Herunterladen von Webseiten vollständig zu verstehen.
