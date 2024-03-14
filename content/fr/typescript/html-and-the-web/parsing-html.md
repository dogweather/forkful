---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:11.716447-07:00
description: "Parser du HTML signifie fouiller dans le code HTML pour trouver, extraire\
  \ ou manipuler des informations. Les programmeurs le font pour interagir avec le\u2026"
lastmod: '2024-03-13T22:44:57.434518-06:00'
model: gpt-4-0125-preview
summary: "Parser du HTML signifie fouiller dans le code HTML pour trouver, extraire\
  \ ou manipuler des informations. Les programmeurs le font pour interagir avec le\u2026"
title: Analyse Syntaxique du HTML
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Parser du HTML signifie fouiller dans le code HTML pour trouver, extraire ou manipuler des informations. Les programmeurs le font pour interagir avec le contenu web—peut-être en grattant des données, ou en automatisant des navigateurs.

## Comment faire :

Pour commencer, installez une bibliothèque comme `node-html-parser`. Voici la commande de terminal :

```bash
npm install node-html-parser
```

Maintenant, analysons un peu de HTML basique en TypeScript :

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Pomme</li>
                <li>Banane</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Pomme Banane"
```

Et si vous voulez juste saisir les bananes :

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banane"
```

## Plongée profonde

Parser du HTML n'est pas nouveau—cela existe depuis les premiers jours du web. Initialement, les développeurs auraient pu utiliser des expressions régulières, mais cela est devenu compliqué rapidement. Entrez le DOM Parser : stable, mais lié au navigateur.

Des bibliothèques comme `node-html-parser` éliminent la douleur. Elles vous permettent d'interroger le HTML comme vous le feriez avec jQuery, mais côté serveur avec Node.js. C'est rapide, tolérant au HTML sale, et sympathique au DOM.

Il y a aussi `jsdom`, simulant un environnement de navigateur entier. C'est plus lourd mais plus complet, créant un modèle d'objet de document (DOM) complet pour la manipulation et l'interaction.

N'oublions pas Cheerio, non plus. Il mélange la vitesse avec une syntaxe similaire à jQuery et une empreinte plus petite, se positionnant heureusement entre les deux.

## Voir aussi

Si vous avez soif de plus, plongez dans ceux-ci :
- [Spécification W3C de l'analyse et de la sérialisation du DOM](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser sur GitHub](https://github.com/taoqf/node-html-parser)
- [Dépôt GitHub jsdom](https://github.com/jsdom/jsdom)
- [Site Web Cheerio](https://cheerio.js.org/)
