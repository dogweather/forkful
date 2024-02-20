---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:26.645593-07:00
description: "Att parsa HTML inneb\xE4r att s\xE5lla igenom HTML-kod f\xF6r att hitta,\
  \ extrahera eller manipulera information. Programmerare g\xF6r det f\xF6r att interagera\
  \ med\u2026"
lastmod: 2024-02-19 22:04:56.855294
model: gpt-4-0125-preview
summary: "Att parsa HTML inneb\xE4r att s\xE5lla igenom HTML-kod f\xF6r att hitta,\
  \ extrahera eller manipulera information. Programmerare g\xF6r det f\xF6r att interagera\
  \ med\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa HTML innebär att sålla igenom HTML-kod för att hitta, extrahera eller manipulera information. Programmerare gör det för att interagera med webbinnehåll — kanske genom att skrapa data eller automatisera webbläsare.

## Hur man gör:

För att komma igång, installera ett bibliotek som `node-html-parser`. Här är terminalkommandot:

```bash
npm install node-html-parser
```

Nu ska vi parsa lite grundläggande HTML i TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

Och om du bara vill ta tag i bananerna:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## Fördjupning

Att parsa HTML är inte nytt — det har funnits sedan webbens tidiga dagar. Inledningsvis kanske utvecklare använde reguljära uttryck, men det blev snabbt rörigt. Då kom DOM-parsern: stabil, men begränsad till webbläsare.

Bibliotek som `node-html-parser` abstraherar bort smärtan. De låter dig fråga HTML som du skulle med jQuery, men på serversidan med Node.js. Det är snabbt, tolerant mot smutsig HTML och DOM-vänligt.

Det finns också `jsdom`, som simulerar en hel webbläsarmiljö. Det är tyngre men mer genomgående, skapar ett fullständigt Document Object Model (DOM) för manipulation och interaktion.

Vi får inte glömma Cheerio heller. Det blandar hastighet med en jQuery-liknande syntax och mindre fotavtryck, och sitter lyckligt mellan de två.

## Se även

Om du är törstig efter mer, dyk ner i dessa:
- [DOM Parsing and Serialization W3C Specification](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser på GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub Repository](https://github.com/jsdom/jsdom)
- [Cheerio Webbplats](https://cheerio.js.org/)
