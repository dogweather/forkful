---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:53.479595-07:00
description: "Comment : Cr\xE9er et utiliser des tableaux associatifs (objets) en\
  \ JavaScript est simple. Vous d\xE9finissez un objet avec des accolades `{}`, et\
  \ \xE0 l'int\xE9rieur\u2026"
lastmod: '2024-03-13T22:44:58.267247-06:00'
model: gpt-4-0125-preview
summary: "Cr\xE9er et utiliser des tableaux associatifs (objets) en JavaScript est\
  \ simple."
title: Utilisation des tableaux associatifs
weight: 15
---

## Comment :
Créer et utiliser des tableaux associatifs (objets) en JavaScript est simple. Vous définissez un objet avec des accolades `{}`, et à l'intérieur de ces dernières, vous pouvez définir un ensemble de paires clé-valeur. Les clés sont toujours des chaînes de caractères, et les valeurs peuvent être n'importe quoi : des chaînes, des nombres, des tableaux, voire d'autres objets.

```javascript
// Création d’un tableau associatif
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Accès aux éléments
console.log(userInfo.name); // Sortie : Alex
console.log(userInfo["email"]); // Sortie : alex@example.com

// Ajout de nouveaux éléments
userInfo.job = "Développeur";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Sortie :
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Développeur",
  country: "Canada"
}
*/

// Suppression d'un élément
delete userInfo.age;
console.log(userInfo);
/* Sortie :
{
  name: "Alex",
  email: "alex@example.com",
  job: "Développeur",
  country: "Canada"
}
*/
```

Comme vous pouvez le voir, accéder, ajouter ou supprimer des éléments dans un tableau associatif est assez direct et intuitif.

## Plongée en profondeur
Dans le monde de JavaScript, bien que nous entendons souvent le terme "tableau associatif", c'est techniquement un abus de langage car JavaScript n'a pas de vrais tableaux associatifs comme d'autres langages (par exemple, PHP). Ce que JavaScript a, ce sont des objets qui servent un but similaire mais sont une construction plus puissante et flexible.

Historiquement, les tableaux dans les langages de programmation étaient conçus pour contenir une collection d'articles, accessibles par leur indice numérique. Cependant, à mesure que le développement logiciel évoluait, le besoin de structures de données plus flexibles est apparu. Les tableaux associatifs, ou dictionnaires dans d'autres langages, étaient l'une des réponses, permettant l'accès aux éléments par des clés arbitraires.

L'approche de JavaScript avec les objets comme magasins clé-valeur offre une combinaison de fonctionnalités. Elle permet aux propriétés (clés) d'être ajoutées, supprimées et recherchées par nom. JSON (JavaScript Object Notation) est un témoignage de l'utilité de cette structure, devenant le standard de facto pour l'échange de données sur le web.

Alors que les objets couvrent la plupart des besoins pour les tableaux associatifs, dans les cas où l'ordre des clés ou l'itération est important, l'objet `Map` introduit dans ES6 offre une meilleure alternative. Un `Map` conserve l'ordre des clés, accepte une plus large gamme de types de données comme clés et inclut des méthodes utiles pour l'itération et la récupération de la taille. Malgré ces avantages, la syntaxe traditionnelle des objets reste populaire pour sa simplicité et sa facilité d'utilisation dans de nombreux scénarios courants.
