---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:12.985819-07:00
description: 'Wie geht das: Um zu beginnen, installieren Sie eine Bibliothek wie `node-html-parser`.
  Hier ist der Terminalbefehl.'
lastmod: '2024-03-13T22:44:53.629240-06:00'
model: gpt-4-0125-preview
summary: Um zu beginnen, installieren Sie eine Bibliothek wie `node-html-parser`.
title: HTML parsen
weight: 43
---

## Wie geht das:
Um zu beginnen, installieren Sie eine Bibliothek wie `node-html-parser`. Hier ist der Terminalbefehl:

```bash
npm install node-html-parser
```

Jetzt lassen Sie uns etwas grundlegendes HTML in TypeScript parsen:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apfel</li>
                <li>Banane</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apfel Banane"
```

Und wenn Sie nur die Bananen nehmen wollen:

```typescript
const bananen = root.querySelectorAll('li')[1].textContent;
console.log(bananen);  // "Banane"
```

## Tiefergehend
HTML zu parsen ist nicht neu – es gibt dies seit den frühen Tagen des Webs. Anfangs haben Entwickler vielleicht reguläre Ausdrücke verwendet, aber das wurde schnell unübersichtlich. Dann kam der DOM Parser: stabil, aber an den Browser gebunden.

Bibliotheken wie `node-html-parser` nehmen Ihnen die Schmerzen ab. Sie ermöglichen es Ihnen, HTML abzufragen, wie Sie es mit jQuery tun würden, aber serverseitig mit Node.js. Es ist schnell, tolerant gegenüber schmutzigem HTML und DOM-freundlich.

Es gibt auch `jsdom`, das eine gesamte Browserumgebung simuliert. Es ist schwerer, aber gründlicher und erstellt ein vollständiges Document Object Model (DOM) zur Manipulation und Interaktion.

Vergessen wir auch Cheerio nicht. Es vereint Geschwindigkeit mit einer jQuery-ähnlichen Syntax und kleinerem Fußabdruck und sitzt glücklich zwischen den beiden.

## Siehe auch
Wenn Sie nach mehr dürsten, tauchen Sie hier ein:
- [DOM-Parsing und Serialisierung W3C-Spezifikation](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser auf GitHub](https://github.com/taoqf/node-html-parser)
- [jsdom GitHub-Repository](https://github.com/jsdom/jsdom)
- [Cheerio-Website](https://cheerio.js.org/)
