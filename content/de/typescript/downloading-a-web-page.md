---
title:                "Webseite herunterladen"
aliases:
- de/typescript/downloading-a-web-page.md
date:                  2024-01-20T17:45:04.691204-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Herunterladen einer Webseite bedeutet, ihre Inhalte programmgesteuert abzurufen, um sie zu verarbeiten oder offline zu speichern. Programmierer machen das häufig, um Daten zu sammeln oder automatisch auf Webinhalte zu reagieren.

## How to: (Wie geht das:)
```TypeScript
import axios from 'axios';

async function downloadWebPage(url: string): Promise<string> {
    try {
        const response = await axios.get(url);
        return response.data;
    } catch (error) {
        console.error('Download failed:', error);
        return '';
    }
}

// Beispiel für die Verwendung der Funktion
(async () => {
    const url = 'https://example.com';
    const content = await downloadWebPage(url);
    console.log(content);
})();
```
Sample output (Beispiel für Ausgaben):
```plaintext
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>
```

## Deep Dive (Tiefer Eintauchen):
Das Downloaden von Webseiten begann in den Anfangstagen des Internets. Man bediente sich des `HTTP GET`-Requests, um Webinhalte abzurufen. Frühere Methoden verwendeten Tools wie `curl` oder `wget` im Terminal oder spezielle Bibliotheken wie `requests` in Python.

In TypeScript gibt es mehrere Möglichkeiten, eine Webseite herunterzuladen: `axios`, `node-fetch` oder die eingebauten `http` und `https` Modulen von Node.js. `Axios` wird häufig gewählt, da es eine klare API bietet und sowohl in Node.js als auch im Browser funktioniert. Alternativ könntest du die `fetch` API nutzen, die in modernen Browsern eingebaut ist und mit polyfills in Node.js verfügbar gemacht werden kann.

Wenn es um die Implementierung geht, ist es wichtig, Fehler zu behandeln, wie im Beispiel gezeigt. Netzwerkoperationen können fehlschlagen oder Inhalte können nicht das erwartete Format haben. Effektive Fehlerbehandlung sorgt dafür, dass dein Programm robust gegen solche Eventualitäten ist.

## See Also (Siehe auch):
- [Axios GitHub Repository](https://github.com/axios/axios)
- [MDN Web Docs: Using Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [Node.js HTTP Module](https://nodejs.org/api/http.html)
- [Fetch Polyfill for Node.js](https://github.com/node-fetch/node-fetch)
