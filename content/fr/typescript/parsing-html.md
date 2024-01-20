---
title:                "Analyser le HTML"
html_title:           "Kotlin: Analyser le HTML"
simple_title:         "Analyser le HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-html.md"
---

{{< edit_this_page >}}

# Analyse HTLM avec TypeScript: Un guide simple et clair

## Quoi & Pourquoi?

L'analyse de HTML, c'est le processus de transformer le code HTML en une structure de données plus facile à manipuler pour votre programme. Nous faisons cela pour extraire des informations, manipuler des éléments ou interagir de manière plus sophistiquée avec nos pages Web.

## Comment faire:

Avec TypeScript, nous allons utiliser une bibliothèque appelée `jsdom`. Voici comment on peut l'utiliser pour analyser un fragment de HTML.

```TypeScript
import { JSDOM } from 'jsdom';

let dom = new JSDOM(`<!DOCTYPE html><p>Hello world</p>`);
console.log(dom.window.document.querySelector("p").textContent); // "Hello world"
```

Dans cet exemple, nous avons créé un nouveau JSDOM et recherché un élément paragraphe à travers `querySelector`. Le `textContent` nous donne le contenu de cet élément.

## Plongée Profonde

Historiquement, l'analyse de HTML était beaucoup plus compliquée et moins flexible. Il y avait un besoin d'outils plus sophistiqués et modernes, et c'est pourquoi `jsdom` a été créé.

Il existe d'autres alternatives à `jsdom`, comme `cheerio`, qui est plus rapide mais ne prend en charge que le sous-ensemble de jQuery. Si vous n'avez pas besoin d'une simulation de navigateur complète (que `jsdom` offre), `cheerio` pourrait être une meilleure option.

En parlant de l'implémentation, `jsdom` utilise document.implementation.createHTMLDocument() pour parser le HTML en un objet DOM. Cette fonction est une API standard du navigateur, ce qui rend notre code beaucoup plus fiable.

## Voir Aussi

1. Documentation de Jsdom: [https://github.com/jsdom/jsdom](https://github.com/jsdom/jsdom)
2. Articles sur le parsing de HTML avec jsdom: [https://dev.to/thawkin3/jsdom-vs-cheerio-15ic](https://dev.to/thawkin3/jsdom-vs-cheerio-15ic)
3. API de CreateHTMLDocument() : [https://developer.mozilla.org/fr/docs/Web/API/DOMImplementation/createHTMLDocument](https://developer.mozilla.org/fr/docs/Web/API/DOMImplementation/createHTMLDocument)