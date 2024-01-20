---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
Analyser du HTML, c'est transformer une chaîne de texte HTML en une structure de donnée. Les programmeurs le font pour manipuler, extraire des données, ou intégrer des scripts dans les pages web.

## Comment faire :
Voici un exemple simple avec la bibliothèque JSDOM qui nous permet de parser du HTML :

```Javascript
const jsdom = require("jsdom");
const { JSDOM } = jsdom;
const dom = new JSDOM(`<!DOCTYPE html><body><h1>Hello world</h1></body></html>`);
console.log(dom.window.document.querySelector("h1").textContent); // Outputs: 'Hello world'
```
Ici, nous avons créé une nouvelle instance JSDOM et nous avons analysé une simple chaîne HTML. Ensuite, nous avons utilisé la méthode querySelector pour sélectionner la balise h1 et afficher son contenu.

## Plongée en profondeur
Historiquement, l'analyse de HTML était compliquée et inefficace. Mais avec l'évolution des technologies, des bibliothèques comme JSDOM ou Cheerio ont radicalement simplifié le processus.

En termes d'alternatives, vous pourriez utiliser des expressions régulières, bien que cela peut être délicat et sujet à des erreurs. Vous avez aussi des techniques plus récentes comme l'utilisation de l'API Fetch et DOMParser dans un environnement de navigateur.

D'un point de vue technique, les parseurs HTML parcourent chaque caractère de la chaîne HTML et produisent un arbre DOM (Document Object Model). Il s'agit d'une représentation structurée de votre document.

## A voir aussi
N'hésitez pas à approfondir le sujet avec les liens ci-dessous:
- [JSDOM sur GitHub](https://github.com/jsdom/jsdom)
- [Cheerio sur GitHub](https://github.com/cheeriojs/cheerio)
- [MDN Web Docs sur querySelector](https://developer.mozilla.org/fr/docs/Web/API/Document/querySelector)
- [MDN Web Docs sur DOMParser](https://developer.mozilla.org/fr/docs/Web/API/DOMParser)