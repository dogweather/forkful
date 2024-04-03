---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:25.348863-07:00
description: "Comment faire : Dans Google Apps Script, l'interpolation de cha\xEE\
  nes est r\xE9alis\xE9e \xE0 travers les litt\xE9raux de gabarit. Il s'agit de litt\xE9\
  raux de cha\xEEnes\u2026"
lastmod: '2024-03-13T22:44:57.167669-06:00'
model: gpt-4-0125-preview
summary: "Dans Google Apps Script, l'interpolation de cha\xEEnes est r\xE9alis\xE9\
  e \xE0 travers les litt\xE9raux de gabarit."
title: "Interpolation d'une cha\xEEne de caract\xE8res"
weight: 8
---

## Comment faire :
Dans Google Apps Script, l'interpolation de chaînes est réalisée à travers les littéraux de gabarit. Il s'agit de littéraux de chaînes permettant des expressions intégrées, signalées par des accents graves (\`) au lieu des guillemets habituels. Voici comment vous pouvez les utiliser :

```javascript
// Un exemple basique
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Bonjour, ${user} !`); // Affichage : Bonjour, Alice !
}

// Utilisant des expressions
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Cinq plus dix égale ${a + b}.`); // Affichage : Cinq plus dix égale 15.
}

// Chaînes sur plusieurs lignes
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Ceci est une chaîne sur plusieurs lignes :
Bonjour à tous,
Nous discutons de ${item} aujourd'hui.`);
  // Affichage :
  // Ceci est une chaîne sur plusieurs lignes :
  // Bonjour à tous,
  // Nous discutons de Google Apps Script aujourd'hui.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Ces exemples illustrent l'utilisation basique, l'intégration d'expressions et la création de chaînes sur plusieurs lignes avec des valeurs interpolées.

## Plongée Profonde
Les littéraux de gabarit, incluant l'interpolation de chaînes, ont été introduits dans ECMAScript 2015 (ES6) et par la suite adoptés dans Google Apps Script. Avant cela, les programmeurs devaient se fier purement à la concaténation de chaînes, ce qui pouvait devenir lourd pour les chaînes complexes ou lors de l'intégration de nombreuses valeurs de variables.

```javascript
// Ancienne méthode (avant ES6)
var user = 'Bob';
console.log('Bonjour, ' + user + ' !');
```

Bien que l'interpolation de chaînes soit une fonctionnalité puissante, il est important de faire attention aux contextes dans lesquels elle est utilisée. Par exemple, l'intégration directe d'entrées utilisateur sans une sanitisation adéquate peut entraîner des problèmes de sécurité, tels que des attaques par injection. Les développeurs de Google Apps Script doivent s'assurer que tout contenu dynamique interpolé dans des chaînes est correctement vérifié ou assaini.

En comparaison avec d'autres langages de programmation, le concept d'interpolation de chaînes existe largement, avec une syntaxe variable. Python utilise les f-strings ou la méthode `format`, Ruby utilise `#{}` à l'intérieur des chaînes entre guillemets doubles, et de nombreux langages modernes ont adopté des fonctionnalités similaires en raison de la lisibilité et de la commodité qu'elles offrent.

Bien que Google Apps Script n'offre pas de fonctionnalités d'interpolation supplémentaires au-delà de celles fournies par les normes ECMAScript, la fonctionnalité présente est puissante et suffisante pour la plupart des cas d'utilisation. Les développeurs venant de langages avec des mécanismes d'interpolation plus élaborés peuvent avoir besoin d'ajuster leurs attentes mais apprécieront probablement la simplicité et l'efficacité des littéraux de gabarit dans Google Apps Script.
