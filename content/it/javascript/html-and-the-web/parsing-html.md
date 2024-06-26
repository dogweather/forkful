---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:00:42.922727-07:00
description: 'Come fare: Analizziamo l''HTML utilizzando l''API `DOMParser` in JavaScript.'
lastmod: '2024-03-13T22:44:43.809881-06:00'
model: gpt-4-0125-preview
summary: Analizziamo l'HTML utilizzando l'API `DOMParser` in JavaScript.
title: Analisi sintattica HTML
weight: 43
---

## Come fare:
Analizziamo l'HTML utilizzando l'API `DOMParser` in JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Ciao, mondo!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Output: Ciao, mondo!
```

Ora, prendiamo qualcosa di più specifico, come un elemento con una classe:

```Javascript
const htmlString = `<div><p class="greeting">Ciao, di nuovo!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Output: Ciao, di nuovo!
```

## Approfondimento
L'analisi dell'HTML è vecchia quanto il web. Inizialmente, era una cosa dei browser: i browser analizzavano l'HTML per visualizzare le pagine web. Col tempo, i programmatori hanno voluto partecipare a questo processo, portando a API come `DOMParser`.

Alternative? Certamente. Abbiamo librerie come `jQuery` e strumenti come `BeautifulSoup` per Python. Ma il `DOMParser` nativo di JavaScript è veloce e integrato, non c'è bisogno di librerie aggiuntive.

Per quanto riguarda l'implementazione, quando si analizza l'HTML con `DOMParser`, viene creato un oggetto `Document`. Pensalo come un modello gerarchico del tuo HTML. Una volta che lo hai, puoi navigarlo e manipolarlo proprio come faresti con il DOM di una normale pagina web.

Ecco il punto: l'analisi può incappare in HTML malformato. I browser sono tolleranti, ma `DOMParser` potrebbe non esserlo. Quindi, per compiti complessi o HTML disordinato, le librerie di terze parti potrebbero fare un lavoro di pulizia migliore.

## Vedi Anche
- Documentazione MDN sull'API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/it/docs/Web/API/DOMParser)
- Capacità di parsing di jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, un'implementazione veloce, flessibile e snella del core di jQuery per il server: [Cheerio.js](https://cheerio.js.org/)
- Per l'analisi non-JS: la libreria BeautifulSoup di Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
