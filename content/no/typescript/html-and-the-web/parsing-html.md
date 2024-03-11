---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.801948-07:00
description: "\xC5 parse HTML betyr \xE5 sile gjennom HTML-kode for \xE5 finne, hente\
  \ ut, eller manipulere informasjon. Programmerere gj\xF8r dette for \xE5 samhandle\
  \ med webinnhold \u2013\u2026"
lastmod: '2024-03-11T00:14:14.061034-06:00'
model: gpt-4-0125-preview
summary: "\xC5 parse HTML betyr \xE5 sile gjennom HTML-kode for \xE5 finne, hente\
  \ ut, eller manipulere informasjon. Programmerere gj\xF8r dette for \xE5 samhandle\
  \ med webinnhold \u2013\u2026"
title: Analysering av HTML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML betyr å sile gjennom HTML-kode for å finne, hente ut, eller manipulere informasjon. Programmerere gjør dette for å samhandle med webinnhold – kanskje skrape data eller automatisere nettlesere.

## Hvordan:

For å komme i gang, installer et bibliotek som `node-html-parser`. Her er terminalkommandoen:

```bash
npm install node-html-parser
```

Nå, la oss parse noe grunnleggende HTML i TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Eple</li>
                <li>Banan</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Eple Banan"
```

Og hvis du vil gripe bare bananene:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banan"
```

## Dypdykk

Å parse HTML er ikke nytt – det har vært rundt siden webens tidlige dager. I begynnelsen kan utviklere ha brukt regulære uttrykk, men det ble fort rotete. Så kom DOM Parser: stabil, men begrenset til nettlesere.

Biblioteker som `node-html-parser` abstraherer bort smerten. De lar deg forespørre HTML som du ville med jQuery, men server-side med Node.js. Det er raskt, tolerant til skitten HTML, og DOM-vennlig.

Det er også `jsdom`, som simulerer et helt nettlesermiljø. Det er tyngre, men mer grundig, og skaper en fullblåst Document Object Model (DOM) for manipulering og interaksjon.

Vi må heller ikke glemme Cheerio. Det blander hastighet med en jQuery-lignende syntaks og mindre fotavtrykk, og sitter lykkelig mellom de to.

## Se Også

Hvis du er tørst etter mer, dykk inn i disse:
- [DOM Parsing og Serialisering W3C Spesifikasjon](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser på GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub Repository](https://github.com/jsdom/jsdom)
- [Cheerio Nettsted](https://cheerio.js.org/)
