---
title:    "TypeScript: Calculer une date dans le futur ou le passé"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile de pouvoir calculer une date dans le passé ou dans le futur dans un logiciel. Cela peut être pour afficher des rappels pour les événements à venir ou pour calculer une date de livraison prévue pour une commande. Dans cet article, nous explorerons comment le faire en utilisant le langage de programmation TypeScript.

## Comment faire

Nous pouvons utiliser la bibliothèque standard JavaScript `Date` pour créer et manipuler des objets de date en TypeScript. Pour calculer une date dans le futur, nous pouvons utiliser la méthode `setDate()` pour définir une date et utiliser la méthode `getDate()` pour récupérer la date calculée. Par exemple:

```
TypeScript
const currentDate: Date = new Date();
const futureDate: Date = currentDate.setDate(currentDate.getDate() + 7);  // ajouter 7 jours à la date actuelle
console.log(futureDate);  // affiche la date calculée dans 7 jours
```

De même, pour calculer une date dans le passé, nous pouvons utiliser la méthode `setDate()` en passant un nombre négatif pour soustraire des jours à la date actuelle. Par exemple:

```
TypeScript
const currentDate: Date = new Date();
const pastDate: Date = currentDate.setDate(currentDate.getDate() - 3);  // soustraire 3 jours à la date actuelle
console.log(pastDate);  // affiche la date calculée il y a 3 jours
```

Nous pouvons également utiliser d'autres méthodes pour ajuster le temps, comme `setMonth()` pour définir un mois ou `setFullYear()` pour définir une année. En combinant ces méthodes avec `getDate()`, nous pouvons calculer des dates dans le futur ou dans le passé avec précision.

## Plongée en profondeur

Il est important de noter que la classe `Date` en JavaScript commence à compter les mois à partir de 0, donc le mois de janvier est représenté par 0 et le mois de décembre par 11. Cela peut causer des erreurs lors du calcul des dates dans le futur ou dans le passé, donc il est important de garder cela à l'esprit lors de l'utilisation de cette méthode.

En outre, la méthode `setDate()` modifiera réellement l'objet `Date` d'origine, donc si vous avez besoin de la date d'origine pour d'autres calculs, assurez-vous de créer une nouvelle instance de `Date` plutôt que de travailler directement sur celle d'origine.

## Voir aussi

- [Documentation JavaScript Date](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Introduction à TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Syntaxe de base de TypeScript](https://www.tutorialspoint.com/typescript/typescript_syntax.htm)