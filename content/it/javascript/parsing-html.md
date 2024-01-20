---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:32:28.731584-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML significa estrarre dati da una struttura HTML. Lo facciamo per manipolare il DOM, ottenere informazioni da pagine web o per scrapare dati da siti esterni.

## How to:
Esempio con `DOMParser`:

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<p>Ciao mondo!</p>', 'text/html');
console.log(doc.body.textContent); // Output: Ciao mondo!
```

Esempio con `innerHTML`:

```Javascript
let container = document.createElement('div');
container.innerHTML = '<p>Ciao mondo!</p>';
console.log(container.firstChild.textContent); // Output: Ciao mondo!
```

## Deep Dive
Il parsing di HTML ha una storia abbastanza ricca. Nasce dalla necessità di leggere documenti HTML in modi non previsti dai semplici browser web. Alcuni tool storici includono `BeautifulSoup` per Python e `Nokogiri` per Ruby. In JavaScript, abbiamo visto soluzioni come `jQuery`, che offre metodi per manipolare il DOM facilmente.

Il parsing HTML in JavaScript è diventato più semplice con l'introduzione di `DOMParser` e l'API Fetch. `DOMParser` permette di analizzare stringhe HTML e costruire un `Document` che rappresenta il DOM, mentre Fetch può aiutarci a ottenere HTML da fonti remote.

Gli sviluppatori dovrebbero essere attenti con la sicurezza quando si effettua il parsing di HTML, specialmente con dati non affidabili che potrebbero portare a attacchi XSS. Assicurati sempre di sanificare l'HTML prima di usarlo.

## See Also
- MDN Web Docs su DOMParser: https://developer.mozilla.org/it/docs/Web/API/DOMParser
- OWASP per pratiche sicure di parsing HTML: https://owasp.org/www-community/attacks/xss/
- Esempi pratici con Fetch API: https://developer.mozilla.org/it/docs/Web/API/Fetch_API/Using_Fetch