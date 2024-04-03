---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:49.623689-07:00
description: "HTML parseren betekent het doorzoeken van HTML-code om informatie te\
  \ vinden, te extraheren of te manipuleren. Programmeurs doen dit om te interageren\
  \ met\u2026"
lastmod: '2024-03-13T22:44:50.548257-06:00'
model: gpt-4-0125-preview
summary: HTML parseren betekent het doorzoeken van HTML-code om informatie te vinden,
  te extraheren of te manipuleren.
title: HTML Parsen
weight: 43
---

## Hoe te beginnen:
Om te beginnen, installeer je een bibliotheek zoals `node-html-parser`. Hier is de terminalopdracht:

```bash
npm install node-html-parser
```

Laten we nu wat basis HTML in TypeScript parsen:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Appel</li>
                <li>Banaan</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Appel Banaan"
```

En als je alleen de bananen wilt pakken:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banaan"
```

## Diepgaand
HTML parseren is niet nieuw—het bestaat al sinds de vroege dagen van het web. Aanvankelijk zouden ontwikkelaars reguliere expressies kunnen hebben gebruikt, maar dat werd snel rommelig. Enter de DOM Parser: stabiel, maar gebonden aan de browser.

Bibliotheken zoals `node-html-parser` nemen de pijn weg. Ze laten je HTML bevragen zoals je dat met jQuery zou doen, maar dan server-side met Node.js. Het is snel, tolerant voor vieze HTML, en DOM-vriendelijk.

Er is ook `jsdom`, dat een volledige browseromgeving simuleert. Het is zwaarder maar grondiger, en creëert een volwaardig Document Object Model (DOM) voor manipulatie en interactie.

Laten we Cheerio ook niet vergeten. Het combineert snelheid met een jQuery-achtige syntaxis en een kleinere voetafdruk, en zit mooi tussen de twee in.

## Zie Ook
Als je dorstig bent naar meer, duik dan in deze:
- [DOM Parsing and Serialization W3C Specification](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser op GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub Repository](https://github.com/jsdom/jsdom)
- [Cheerio Website](https://cheerio.js.org/)
