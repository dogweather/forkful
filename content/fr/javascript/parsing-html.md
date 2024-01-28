---
title:                "Analyse Syntaxique du HTML"
date:                  2024-01-28T03:00:43.691521-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse (parsing) HTML consiste à extraire des données des documents HTML. Les programmeurs le font pour interagir avec ou manipuler le contenu web, automatiser l'extraction de données, ou pour des fins de scraping web.

## Comment faire :
Analysons le HTML en utilisant l'API `DOMParser` en JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Bonjour, monde !</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Sortie: Bonjour, monde !
```

Maintenant, attrapons quelque chose de plus spécifique, comme un élément avec une classe :

```Javascript
const htmlString = `<div><p class="greeting">Bonjour, encore !</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Sortie: Bonjour, encore !
```

## Plongée Profonde
L'analyse HTML est aussi ancienne que le web. Initialement, c'était une chose de navigateur — les navigateurs analysaient le HTML pour afficher les pages web. Avec le temps, les programmeurs ont voulu s'immiscer dans ce processus, conduisant à des API comme `DOMParser`.

Des alternatives ? Bien sûr. Nous avons des bibliothèques comme `jQuery` et des outils comme `BeautifulSoup` pour Python. Mais le `DOMParser` natif de JavaScript est rapide et intégré, pas besoin de bibliothèques supplémentaires.

En termes d'implémentation, lorsque vous analysez du HTML avec `DOMParser`, cela crée un objet `Document`. Pensez-y comme à un modèle hiérarchique de votre HTML. Une fois que vous l'avez, vous pouvez le naviguer et le manipuler tout comme vous le feriez avec le DOM d'une page web normale.

Voici le truc — l'analyse peut trébucher sur du HTML mal formé. Les navigateurs sont indulgents, mais le `DOMParser` pourrait ne pas l'être. Ainsi, pour des tâches complexes ou du HTML désordonné, des bibliothèques tierces pourraient faire un meilleur travail de nettoyage.

## Voir Aussi
- MDN Web Docs sur l'API `DOMParser` : [MDN DOMParser](https://developer.mozilla.org/fr/docs/Web/API/DOMParser)
- Les capacités d'analyse de jQuery : [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, une implémentation rapide, flexible et épurée du cœur de jQuery pour le serveur : [Cheerio.js](https://cheerio.js.org/)
- Pour l'analyse hors JS : la bibliothèque BeautifulSoup de Python : [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
