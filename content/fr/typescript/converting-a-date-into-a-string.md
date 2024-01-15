---
title:                "Transformer une date en chaîne de caractères"
html_title:           "TypeScript: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il serait utile de convertir une date en chaine de caractères dans certaines situations, par exemple pour afficher une date sous un format spécifique ou pour faciliter la manipulation de dates en tant que chaînes de caractères dans un code TypeScript.

## Comment faire

```TypeScript
// Exemple 1 : Utilisation de la fonction toDateString()
let date = new Date();
let dateString = date.toDateString();

console.log(dateString); // Output : Fri Jul 30 2021

// Exemple 2 : Utilisation de la méthode toLocaleDateString()
let date = new Date();
let dateString = date.toLocaleDateString('fr-FR');

console.log(dateString); // Output : 30/07/2021
```

## Profondeur

Pour comprendre comment fonctionne la conversion d'une date en chaîne de caractères en TypeScript, il est important de comprendre comment les dates sont représentées dans ce langage. En TypeScript, une date est représentée par l'objet `Date` qui contient des propriétés et des méthodes pour manipuler et formater les dates.

Lorsque l'on convertit une date en chaîne de caractères, on utilise généralement les méthodes `toDateString()` ou `toLocaleDateString()`. La première méthode retourne la date sous un format standard tel que "Jeu Jul 29 2021", tandis que la seconde méthode permet de spécifier une localisation pour obtenir le format de date approprié pour cette langue.

Il est également possible de formater une date à l'aide de la classe `DateTimeFormat` qui offre une plus grande flexibilité pour personnaliser le format de la date.

## Voir aussi

- Documentation officielle de TypeScript sur les dates : https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#date
- Un tutoriel pour comprendre les dates en JavaScript (le langage sur lequel est basé TypeScript) : https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date