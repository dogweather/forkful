---
title:                "Analyser le html"
html_title:           "Javascript: Analyser le html"
simple_title:         "Analyser le html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que le parsing HTML et pourquoi les programmeurs le font-ils?

Le parsing HTML est le processus de lecture et d'analyse du code HTML pour le convertir en une structure de données utilisable par le navigateur. Les programmeurs font cela pour permettre à leurs sites web d'être interprétés correctement par les navigateurs et d'afficher le contenu de manière cohérente.

## Comment faire:

Voici un exemple simple de parsing HTML en utilisant Javascript:

```Javascript
const html = "<h1>Bienvenue sur mon site web!</h1>"; // html à parser
const parser = new DOMParser(); // création du parseur
const parsedHtml = parser.parseFromString(html, 'text/html'); // parsing de l'HTML
console.log(parsedHtml.getElementsByTagName('h1')[0].innerText); // affiche "Bienvenue sur mon site web!" dans la console
```

## Plongée en profondeur:

Le parsing HTML existe depuis le début du web, car c'était la façon dont les navigateurs ont commencé à afficher le contenu des sites web. Avant HTML5, le parsing était plutôt rudimentaire et pouvait entraîner des erreurs si le code était mal formé. Maintenant, il existe des alternatives comme JSON, mais le parsing HTML reste la méthode la plus courante pour afficher des pages web.

## Voir aussi:

Pour en savoir plus sur le parsing HTML et Javascript, voici quelques liens utiles:

- [La spécification HTML5](https://www.w3.org/TR/html52/)/
- [La documentation officielle de Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript)
- [Un tutoriel sur le parsing HTML en Javascript](https://devdocs.io/domparser/)