---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:39.020300-07:00
description: "Mettre en majuscule une cha\xEEne de caract\xE8res consiste \xE0 modifier\
  \ le premier caract\xE8re d'une cha\xEEne donn\xE9e en majuscule s'il est en minuscule,\
  \ laissant\u2026"
lastmod: '2024-03-13T22:44:57.418796-06:00'
model: gpt-4-0125-preview
summary: "Mettre en majuscule une cha\xEEne de caract\xE8res consiste \xE0 modifier\
  \ le premier caract\xE8re d'une cha\xEEne donn\xE9e en majuscule s'il est en minuscule,\
  \ laissant\u2026"
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Quoi & Pourquoi ?
Mettre en majuscule une chaîne de caractères consiste à modifier le premier caractère d'une chaîne donnée en majuscule s'il est en minuscule, laissant souvent le reste de la chaîne inchangé. Cette opération est généralement utilisée pour garantir que les noms propres ou les débuts de phrases respectent les règles grammaticales dans le traitement de texte, rendant les sorties professionnelles et lisibles.

## Comment faire :

TypeScript, étant un sur-ensemble de JavaScript, permet diverses méthodes pour mettre des chaînes en majuscule, allant des approches purement JavaScript à l'utilisation de bibliothèques tierces pour des cas d'utilisation plus complexes ou spécifiques.

**Approche Purement JavaScript :**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Exemple de sortie :
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Cette méthode est simple et s'appuie sur la méthode `charAt()` pour accéder au premier caractère de la chaîne et `toUpperCase()` pour le convertir en majuscule. La méthode `slice(1)` récupère ensuite le reste de la chaîne, le laissant inchangé.

**Utilisation de la Bibliothèque Lodash :**

Pour les projets utilisant déjà la bibliothèque [Lodash](https://lodash.com/), vous pouvez utiliser sa fonction `_.capitalize` pour obtenir le même résultat avec moins de code modèle.

Tout d'abord, installez Lodash :

```bash
npm install lodash
```

Ensuite, utilisez-le dans votre fichier TypeScript :

```typescript
import * as _ from 'lodash';

// Exemple de sortie :
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Note : La méthode `_.capitalize` de Lodash convertit le reste de la chaîne en minuscule, ce qui peut ne pas toujours être ce que vous souhaitez.

**Utilisation d'une expression régulière :**

Une expression régulière peut fournir un moyen concis de mettre en majuscule la première lettre d'une chaîne, surtout si vous avez besoin de mettre en majuscule la première lettre de chaque mot dans une chaîne.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Exemple de sortie :
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Cette méthode utilise la fonction `replace()` pour rechercher toute limite de mot suivie d'un caractère alphanumérique (`\b\w`), mettant chaque correspondance en majuscule. Elle est particulièrement pratique pour les titres ou les en-têtes.
