---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:34:17.663870-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parse HTML significa trasformare codice HTML in strutture dati che JavaScript può capire e manipolare. Lo facciamo per interagire dinamicamente con il contenuto di una pagina web, per esempio per estrarre informazioni o aggiornare la UI.

## How to:
```TypeScript
import { parse } from 'node-html-parser';

const html = `<div>Salve Mondo!</div>`;
const root = parse(html);

// Ottieni il primo div
const div = root.firstChild;
console.log(div?.toString()); // Output: <div>Salve Mondo!</div>

// Estrai il testo
const text = div?.innerText;
console.log(text); // Output: Salve Mondo!
```
Il codice sopra mostra come analizzare un semplice snippet HTML e ottenere il testo da esso.

## Deep Dive
L'analisi HTML è stata centrale fin dall'inizio del web, quando era necessaria per mostrare le pagine sui browser. Oggi, abbiamo scelto `node-html-parser` per TypeScript, ma ci sono alternative come `jsdom` che simulano un ambiente DOM completo o `cheerio` che lavora con una sintassi simile a jQuery.

`node-html-parser` è leggero e veloce, particolarmente adatto quando non è necessario un DOM completo. Utilizza l'AST (Abstract Syntax Tree) per analizzare l'HTML e trasformarlo in nodi accessibili che possiamo manipolare con TypeScript.

L'implementazione prevede la gestione dell'HTML standard e non standard, correggendo automaticamente gli errori comuni di markup.

## See Also
- Documentazione `node-html-parser`: https://github.com/taoqf/node-html-parser
- Alternativa `jsdom`: https://github.com/jsdom/jsdom
- Alternativa `cheerio`: https://github.com/cheeriojs/cheerio
- Specifiche del World Wide Web Consortium (W3C) sull'HTML: https://www.w3.org/TR/html52/
