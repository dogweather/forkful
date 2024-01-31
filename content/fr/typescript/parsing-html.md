---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:34:24.432021-07:00
simple_title:         "Analyse syntaxique de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-html.md"
---

{{< edit_this_page >}}

# TypeScript: Parsing HTML Simplifié

## What & Why? (Quoi et Pourquoi ?)
Le parsing HTML, c'est lire et comprendre le code HTML en utilisant un programme. On le fait pour extraire des données, manipuler le contenu, et interagir avec des pages web depuis nos applications.

## How to: (Comment faire :)
Imaginez que l'on veut extraire le titre d'une page web. Avec TypeScript, on utilise souvent la librairie `node-html-parser`. Installons et utilisons-la :

```TypeScript
import { parse } from 'node-html-parser';

async function getTitle(html: string): Promise<string> {
  const root = parse(html);
  const title = root.querySelector('title')?.textContent;
  return title || 'Titre non trouvé';
}

// Utilisation exemple
const htmlContent = '<!DOCTYPE html><html><head><title>Page Exemple</title></head></html>';
getTitle(htmlContent).then(title => console.log(title)); // Affiche: Page Exemple
```

Installation nécessaire:
```bash
npm install node-html-parser
```

## Deep Dive (Plongée en profondeur)
Historiquement, le parsing HTML nécessitait des méthodes complexes et non standardisées. Avec l'évolution de JavaScript et TypeScript, des librairies modernes comme `node-html-parser` simplifient la tâche. Par rapport à `DOMParser` (le choix natif du navigateur), `node-html-parser` est plus flexible et fonctionne aussi côté serveur (Node.js).

On trouve d'autres options comme `cheerio` pour qui préfère une syntaxe proche de jQuery, ou `jsdom` pour simuler un DOM complet en dehors du navigateur.

Les détails d'implémentation incluent la gestion de l'encodage des caractères, les nuances du DOM, et le respect des spécifications HTML5 pour ne pas introduire de vulnérabilités XSS (Cross-Site Scripting) lors de manipulations.

## See Also (Voir également)
- Documentation de `node-html-parser`: [github.com/taoqf/node-html-parser](https://github.com/taoqf/node-html-parser)
- Comparaison des librairies de parsing HTML en Node.js: 
  [www.npmjs.com/search?q=html+parser](https://www.npmjs.com/search?q=html+parser)
- Spécifications de l'HTML5 concernant le parsing: 
  [html.spec.whatwg.org/multipage/parsing.html](https://html.spec.whatwg.org/multipage/parsing.html)
- Informations sur les vulnérabilités XSS: 
  [owasp.org/www-community/attacks/xss/](https://owasp.org/www-community/attacks/xss/)
