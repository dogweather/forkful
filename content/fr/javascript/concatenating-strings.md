---
title:                "Javascript: Concaténation de chaînes de caractères"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une opération courante en programmation Javascript qui consiste à combiner plusieurs chaînes de caractères pour en former une seule. Cela peut être utile pour créer des messages dynamiques, des URL ou tout autre contenu textuel personnalisé. 

## Comment faire

La concaténation de chaînes de caractères peut être réalisée de différentes manières en Javascript. Voici quelques exemples pratiques : 

```Javascript
// Utiliser l'opérateur '+'
let prenom = "Marie";
let nom = "Dupont";
let nomComplet = prenom + " " + nom;
console.log(nomComplet); // Affiche "Marie Dupont"

// Utiliser la méthode concat()
let message = "Bonjour ";
message = message.concat(prenom, ", comment vas-tu ?");
console.log(message); // Affiche "Bonjour Marie, comment vas-tu ?"

// Utiliser les templates littéraux
let ville = "Paris";
let adresse = `Bienvenue à ${ville}.`;
console.log(adresse); // Affiche "Bienvenue à Paris."
```

Il est également possible d'utiliser la méthode `join()` pour concaténer un tableau de chaînes de caractères.

## Plongée plus profonde

Lorsque vous concaténez des chaînes de caractères en Javascript, il est important de comprendre comment les valeurs sont converties en chaînes de caractères. Les nombres et les booléens seront automatiquement convertis en chaînes de caractères lorsqu'ils sont concaténés avec d'autres chaînes. Cependant, les valeurs `null` et `undefined` apparaîtront comme une chaîne vide lorsqu'elles seront concaténées.

Il est également possible de concaténer des chaînes de caractères avec des expressions ou des fonctions, comme dans l'exemple suivant :

```Javascript
let age = 25;
let message = `Vous avez ${age} ans.`;
console.log(message); // Affiche "Vous avez 25 ans."

// Fonction pour afficher un message de bienvenue
function afficherBienvenue(nom) {
    return `Bienvenue ${nom} !`;
}

let nomUtilisateur = "Jean";
console.log(afficherBienvenue(nomUtilisateur)); // Affiche "Bienvenue Jean !"
```

## Voir aussi

- [Documentation MDN sur la concaténation de chaînes de caractères en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Op%C3%A9rateurs/Splicer%C3%A9_pour_le_concart%C3%A9nation)
- [Guide de résolution de problèmes en Javascript: concaténation](https://alligator.io/js/concatenating-strings/)
- [Exercices pratiques sur la concaténation de chaînes de caractères en Javascript](https://www.w3resource.com/javascript-exercises/javascript-string-exercises.php)