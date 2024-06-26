---
date: 2024-01-26 01:11:08.941884-07:00
description: "Comment faire : Historiquement, les langages de programmation imp\xE9\
  ratifs comme les premi\xE8res versions de BASIC ou l'Assembleur manquaient de l'abstraction\u2026"
lastmod: '2024-04-05T21:53:59.683113-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, les langages de programmation imp\xE9ratifs comme les premi\xE8\
  res versions de BASIC ou l'Assembleur manquaient de l'abstraction que les fonctions\
  \ fournissent."
title: Organisation du code en fonctions
weight: 18
---

## Comment faire :
```javascript
// Définir une fonction pour calculer l'aire d'un rectangle
function calculateArea(largeur, hauteur) {
  return largeur * hauteur;
}

// Appeler la fonction et imprimer le résultat
let aire = calculateArea(5, 3);
console.log(aire); // Sortie : 15
```

```javascript
// Regrouper des fonctionnalités connexes à l'aide de fonctions
function saluer(nom) {
  console.log(`Bonjour, ${nom} !`);
}

function adieu(nom) {
  console.log(`Au revoir, ${nom} !`);
}

saluer('Alice'); // Sortie : Bonjour, Alice !
adieu('Bob'); // Sortie : Au revoir, Bob !
```

## Exploration approfondie
Historiquement, les langages de programmation impératifs comme les premières versions de BASIC ou l'Assembleur manquaient de l'abstraction que les fonctions fournissent. Avec le temps, le concept de code modulaire dans des langages comme C a introduit l'idée que décomposer le code en unités (fonctions ou procédures) conduit à une meilleure organisation et une logique plus claire.

En JavaScript, outre les fonctions simples, nous avons les fonctions fléchées depuis ES6 (2015) qui offrent une syntaxe plus concise et sont adaptées pour les fonctions qui ne sont pas des méthodes.

Les alternatives et les améliorations concernant l'organisation du code en JavaScript incluent des approches orientées objet en utilisant des classes, ou des paradigmes de programmation fonctionnelle qui traitent les fonctions comme des citoyens de première classe.

En termes d'implémentation, les fonctions JavaScript supportent les fermetures (closures), offrant un moyen de conserver l'accès au contexte d'une fonction après son exécution, ce qui est puissant pour l'encapsulation et la création de fonctions d'usine, entre autres modèles.

## Voir aussi
- MDN Web Docs sur les Fonctions : https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Functions
- Modèles de conception JavaScript : https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Code Propre JavaScript : https://github.com/ryanmcdermott/clean-code-javascript
