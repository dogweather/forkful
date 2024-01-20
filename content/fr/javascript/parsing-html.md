---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:07.654243-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Le parsing HTML consiste à analyser le contenu d'une page web pour en extraire des informations structurées. Les programmeurs le font pour manipuler, scraper des données ou interagir avec des pages dynamiquement.

## Comment faire :
```Javascript
// Utilisons le DOMParser pour analyser une chaîne HTML simple
let parser = new DOMParser();
let doc = parser.parseFromString('<p>Bonjour le monde!</p>', 'text/html');

console.log(doc.body.textContent); // Sortie: "Bonjour le monde!"

// Et avec jQuery (si déjà intégré dans votre projet)
let html = $('<div><p>Salut à tous!</p></div>');
console.log(html.find('p').text()); // Sortie: "Salut à tous!"
```

## Exploration Plus Profonde :
Autrefois, parsing du HTML évoquait souvent 'innerHTML' ou des méthodes jQuery. C'était avant une compréhension complète de la sécurité et la performance. DOMParser, présenté ici, est la méthode recommandée. 

Il existe d'autres moyens : avec Node.js, on utilise 'cheerio' ou 'jsdom'. Ces bibliothèques offrent plus de flexibilité et des fonctionnalités adaptées au server-side.

Pour finir, comptez toujours sur la sécurité. Le parsing HTML peut mener à des vulnérabilités XSS si mal géré. Nettoyez toujours le contenu avant de l'utiliser.

## Voir Aussi :
- MDN DOMParser:
  [https://developer.mozilla.org/fr/docs/Web/API/DOMParser](https://developer.mozilla.org/fr/docs/Web/API/DOMParser)
- jQuery.parseHTML:
  [https://api.jquery.com/jquery.parsehtml/](https://api.jquery.com/jquery.parsehtml/)
- Node.js 'cheerio':
  [https://cheerio.js.org/](https://cheerio.js.org/)
- Node.js 'jsdom':
  [https://github.com/jsdom/jsdom](https://github.com/jsdom/jsdom)