---
date: 2024-01-20 15:33:59.686760-07:00
description: 'How to: To get started, install a library like `node-html-parser`. Here''s
  the terminal command.'
lastmod: '2024-03-13T22:44:59.856894-06:00'
model: unknown
summary: To get started, install a library like `node-html-parser`.
title: Parsing HTML
weight: 43
---

## How to:
To get started, install a library like `node-html-parser`. Here's the terminal command:

```bash
npm install node-html-parser
```

Now, let's parse some basic HTML in TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

And if you want to grab just the bananas:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## Deep Dive
Parsing HTML isn't new—it's been around since the web's early days. Initially, developers might have used regular expressions, but that got messy fast. Enter the DOM Parser: stable, but browser-bound.

Libraries like `node-html-parser` abstract the pain away. They let you query HTML like you would with jQuery, but server-side with Node.js. It's fast, tolerant to dirty HTML, and DOM-friendly.

There's also `jsdom`, simulating a whole browser environment. It's heavier but more thorough, creating a full-blown Document Object Model (DOM) for manipulation and interaction.

Let's not forget Cheerio, either. It blends speed with a jQuery-like syntax and smaller footprint, sitting happily between the two.

## See Also
If you're thirsty for more, dip into these:
- [DOM Parsing and Serialization W3C Specification](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser on GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub Repository](https://github.com/jsdom/jsdom)
- [Cheerio Website](https://cheerio.js.org/)
