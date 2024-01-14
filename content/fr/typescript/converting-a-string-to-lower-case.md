---
title:    "TypeScript: Convertissement d'une chaîne en minuscule"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Pourquoi
Il y a de nombreuses raisons pour lesquelles vous pourriez vouloir convertir une chaîne de caractères en minuscules en TypeScript. Cela peut vous permettre de rechercher plus facilement dans une liste ou une base de données, d'afficher une chaîne de caractères de manière uniforme ou de manipuler la chaîne de caractères pour des opérations spécifiques.

## Comment faire
Voici quelques exemples de code en TypeScript pour convertir une chaîne de caractères en minuscules :
```typescript
// Déclaration d'une chaîne de caractères
let maChaine: string = "Hello World!";
// Utilisation de la méthode toLowerCase() pour convertir en minuscules
let chaineMinuscule: string = maChaine.toLowerCase();
// Affichage du résultat
console.log(chaineMinuscule); // affichera "hello world!"
```

Vous pouvez également utiliser une boucle pour convertir chaque caractère individuellement en minuscule :
```typescript
let maChaine: string = "Bonjour!";
let chaineMinuscule: string = "";
// Parcours de chaque caractère de la chaîne
for (let i = 0; i < maChaine.length; i++) {
    // Utilisation de la méthode toLowerCase() sur chaque caractère
    chaineMinuscule += maChaine[i].toLowerCase();
}
// Affichage du résultat
console.log(chaineMinuscule); // affichera "bonjour!"
```

## Deep Dive
Lorsque vous utilisez la méthode toLowerCase() en TypeScript, vous devez être conscient de la différence entre les majuscules et les minuscules selon la langue. Par exemple, en français, la lettre "É" minuscule est "é" et non "E". Cela peut affecter le résultat lors de la conversion en minuscules.

Il est également important de noter que la méthode toLowerCase() ne modifie pas la chaîne de caractères d'origine, mais renvoie une nouvelle chaîne de caractères avec les modifications effectuées. Il est donc important de stocker cette nouvelle chaîne dans une variable si vous avez besoin de l'utiliser par la suite.

## Voir aussi
- [Documentation TypeScript sur les chaînes de caractères](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Méthode toLowerCase() en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)
- [Guide de style JavaScript pour les chaînes de caractères en minuscules](https://www.w3schools.com/js/js_conventions.asp)