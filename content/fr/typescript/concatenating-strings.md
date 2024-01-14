---
title:                "TypeScript: Concaténation de chaînes"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est essentielle en programmation TypeScript pour combiner différentes parties de texte en une seule chaîne. Que vous souhaitiez afficher du texte à l'écran, générer des messages personnalisés ou créer des URL dynamiques, la concaténation de chaînes est un outil utile pour vous aider à atteindre votre objectif.

## Comment faire

La concaténation de chaînes peut s'effectuer de différentes manières en TypeScript, en fonction de vos besoins spécifiques. Voici quelques exemples :

```TypeScript
// Utiliser l'opérateur "+" pour concaténer deux chaînes
let prenom = "Jean";
let nom = "Dupont";
let nomComplet = prenom + " " + nom;
console.log(nomComplet); // Output : "Jean Dupont"

// Utiliser la méthode concat() pour concaténer plusieurs chaînes
let message = "Bonjour ".concat(prenom, " !");
console.log(message); // Output : "Bonjour Jean !"
```

Vous pouvez également utiliser la méthode `toString()` pour convertir des valeurs non-string en chaîne de caractères avant de les concaténer. Par exemple :

```TypeScript
// Concaténer une valeur non-string avec une chaîne de caractères
let age = 25;
let message = "J'ai " + age.toString() + " ans.";
console.log(message); // Output : "J'ai 25 ans."
```

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes peut également être utilisée pour formater des chaînes avec des variables. Ceci est possible grâce aux **littéraux de modèle**, qui vous permettent d'incorporer des expressions dans une chaîne en utilisant des backticks (`` ` ``) et des paramètres entourés de `${}`. Voici un exemple :

```TypeScript
// Utiliser des littéraux de modèle pour formater une chaîne avec des variables
let ville = "Paris";
let temperature = 20;
let message = `Aujourd'hui, il fait ${temperature} degrés à ${ville}.`;
console.log(message); // Output : "Aujourd'hui, il fait 20 degrés à Paris."
```

De plus, TypeScript offre également une syntaxe simplifiée pour la concaténation de chaînes avec des variables en utilisant l'opérateur `+=`. Cela peut être utile dans les scripts plus complexes où plusieurs variables doivent être concaténées en une seule chaîne. Par exemple :

```TypeScript
// Utiliser l'opérateur "+=" pour concaténer plusieurs chaînes avec des variables
let prenom = "Marie";
let nom = "Durand";
let age = 30;
let message = "Bonjour ";
message += prenom + " " + nom + ", vous avez " + age + " ans.";
console.log(message); // Output : "Bonjour Marie Durand, vous avez 30 ans."
```

## Voir aussi

- [Documentation sur les chaînes](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Guide de référence sur les littéraux de modèle](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)