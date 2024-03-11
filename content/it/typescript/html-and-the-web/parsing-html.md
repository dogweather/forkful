---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:13.585602-07:00
description: "Analizzare l'HTML significa setacciare il codice HTML per trovare, estrarre\
  \ o manipolare informazioni. I programmatori lo fanno per interagire con i\u2026"
lastmod: '2024-03-11T00:14:16.735388-06:00'
model: gpt-4-0125-preview
summary: "Analizzare l'HTML significa setacciare il codice HTML per trovare, estrarre\
  \ o manipolare informazioni. I programmatori lo fanno per interagire con i\u2026"
title: Analisi del HTML
---

{{< edit_this_page >}}

## Cosa e Perché?

Analizzare l'HTML significa setacciare il codice HTML per trovare, estrarre o manipolare informazioni. I programmatori lo fanno per interagire con i contenuti web—magari per effettuare scraping di dati, o per automatizzare i browser.

## Come fare:

Per iniziare, installa una libreria come `node-html-parser`. Eccoti il comando per il terminale:

```bash
npm install node-html-parser
```

Ora, analizziamo un po' di HTML di base in TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Mela</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Mela Banana"
```

E se vuoi prendere solo le banane:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## Approfondimento

L'analisi dell'HTML non è una novità—esiste da quando sono nati i primi siti web. Inizialmente, gli sviluppatori potrebbero aver usato espressioni regolari, ma la situazione si è complicata rapidamente. Ecco quindi il Parser DOM: stabile, ma legato al browser.

Librerie come `node-html-parser` semplificano la situazione. Ti permettono di interrogare l'HTML come faresti con jQuery, ma lato server con Node.js. Sono veloci, tolleranti all'HTML sporco, e amichevoli con il DOM.

C'è anche `jsdom`, che simula un intero ambiente browser. È più pesante ma più approfondito, creando un Document Object Model (DOM) completo per la manipolazione e l'interazione.

Non dimentichiamoci poi di Cheerio. Combina velocità con una sintassi simile a jQuery e un ingombro minore, collocandosi felicemente tra i due.

## Vedi Anche

Se sei alla ricerca di altro, immergiti in questi:
- [Specifiche W3C per l'Analisi e la Serializzazione del DOM](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser su GitHub](https://github.com/taoqf/node-html-parser)
- [Repository GitHub di jsdom](https://github.com/jsdom/jsdom)
- [Sito Web di Cheerio](https://cheerio.js.org/)
