---
date: 2024-01-20 17:38:00.585021-07:00
description: "En TypeScript, convertir une date en cha\xEEne de caract\xE8res permet\
  \ de la formater pour l'affichage. Les programmeurs font cela pour am\xE9liorer\
  \ la lisibilit\xE9\u2026"
lastmod: '2024-03-13T22:44:57.449177-06:00'
model: gpt-4-1106-preview
summary: "En TypeScript, convertir une date en cha\xEEne de caract\xE8res permet de\
  \ la formater pour l'affichage."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## How to:
TypeScript utilise les méthodes JavaScript pour manipuler les dates. Voici comment transformer une date en string :

```TypeScript
// Obtention de la date actuelle
const now: Date = new Date();

// Conversion en string avec toDateString()
const dateString: string = now.toDateString();
console.log(dateString); // Affiche la date sous la forme "Wed Mar 25 2020"

// Conversion en string avec toISOString()
const isoString: string = now.toISOString();
console.log(isoString); // Affiche la date sous la forme "2020-03-25T14:45:10.000Z"

// Conversion en string avec des options de localisation
const localString: string = now.toLocaleDateString('fr-FR');
console.log(localString); // Affiche la date sous la forme "25/03/2020"
```

## Deep Dive
Historiquement, JavaScript fournit des objets Date basiques. TypeScript, étant un sur-ensemble, utilise les mêmes objets Date avec l'avantage de la vérification de type.

Il y a diverses méthodes pour convertir une date en string en JavaScript et donc en TypeScript:

- `toString()` et `toDateString()` fournissent des formats de date lisible mais non standardisés selon l'implémentation JS du navigateur.
- `toISOString()` renvoie une date au format ISO 8601, utile pour le stockage et les échanges de données.
- `toLocaleDateString()` permet de personnaliser le format de la date en fonction des préférences locales, ce qui est idéal pour l'affichage utilisateur.

En alternative, des bibliothèques comme `Moment.js` ou `date-fns` offrent encore plus de fonctions de formatage, mais TypeScript rend ces outils moins nécessaires avec l'évolution d'ECMAScript et l'ajout de fonctionnalités de formatage de date à Internationalization API (ECMA-402).

## See Also
- MDN Web Docs sur l'objet Date: [https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Documentation TypeScript officielle: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Moment.js: [https://momentjs.com/](https://momentjs.com/)
- Date-fns: [https://date-fns.org/](https://date-fns.org/)
