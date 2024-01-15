---
title:                "Analyse de HTML"
html_title:           "TypeScript: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous êtes peut-être un développeur web en herbe qui souhaite concevoir un site web dynamique en utilisant Angular ou React. Ou peut-être êtes-vous un développeur chevronné à la recherche d'une solution plus efficace pour extraire des données spécifiques d'une page web. Dans les deux cas, l'analyse de HTML peut être un outil précieux pour atteindre votre objectif.

## Comment faire
```TypeScript
// Importer le module 'cheerio' pour l'analyse de HTML
import * as cheerio from 'cheerio';

// Définir le code HTML à analyser
const html = `
<html>
  <body>
    <h1>Titre</h1>
    <p>Paragraphe</p>
    <ul>
      <li>Élément 1</li>
      <li>Élément 2</li>
      <li>Élément 3</li>
    </ul>
  </body>
</html>
`;

// Charger le code HTML dans Cheerio
const $ = cheerio.load(html);

// Utiliser des sélecteurs CSS pour extraire les données
const titre = $('h1').text(); // Renvoie "Titre"
const paragraphe = $('p').text(); // Renvoie "Paragraphe"
const elements = $('li').map((i, el) => $(el).text()).get(); // Renvoie un tableau avec les éléments 1, 2 et 3

// Afficher les résultats
console.log(titre); // Titre
console.log(paragraphe); // Paragraphe
console.log(elements); // ['Élément 1', 'Élément 2', 'Élément 3']
```

## Plongée en profondeur
L'analyse de HTML utilise des sélecteurs CSS pour extraire les données d'un document HTML en utilisant la bibliothèque Cheerio. Ces sélecteurs peuvent cibler des éléments spécifiques avec des attributs, des classes, des balises et même des combinaisons de ces sélecteurs. Cela se révèle très utile lorsqu'il s'agit d'extraire des données d'une page web complexe et de les utiliser dans votre application.

## Voir aussi
- [Documentation officielle de Cheerio](https://cheerio.js.org/)
- [Guide pour débuter avec TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Utiliser TypeScript avec Angular](https://angular.io/guide/typescript-configuration)