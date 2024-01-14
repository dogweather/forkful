---
title:                "Javascript: Convertir une chaîne en minuscule"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en minuscules est une tâche courante en programmation, et ce pour plusieurs raisons. Tout d'abord, cela permet de standardiser les données en évitant les erreurs de comparaison entre majuscules et minuscules. De plus, certains outils ou bibliothèques peuvent avoir des exigences spécifiques pour les chaînes de caractères en minuscules.

## Comment faire

Voici deux méthodes pour convertir une chaîne de caractères en minuscules en utilisant Javascript.

```Javascript
// Méthode 1: Utiliser la fonction toLowerCase()
let string = "Bonjour le Monde";
let lowercaseString = string.toLowerCase();
console.log(lowercaseString); // Output: bonjour le monde
```

```Javascript
// Méthode 2: Utiliser une boucle for avec la fonction charAt()
let string = "Hello World";
let lowercaseString = "";
for (let i = 0; i < string.length; i++) {
  lowercaseString += string.charAt(i).toLowerCase();
}
console.log(lowercaseString); // Output: hello world
```

Pour les deux méthodes, nous pouvons voir que la chaîne de caractères d'origine est transformée en minuscules lors de l'utilisation des fonctions toLowerCase() ou charAt(). Il est important de noter que ces méthodes ne modifient pas la chaîne d'origine, mais retournent plutôt une nouvelle chaîne de caractères en minuscules.

## Plongée en profondeur

En se plongeant dans les détails techniques, on peut comprendre comment ces méthodes fonctionnent. La fonction toLowerCase() retourne une nouvelle chaîne de caractères avec toutes les lettres en minuscules. Elle utilise l'Unicode pour effectuer cette transformation, ce qui signifie que cela fonctionnera avec toutes les langues prises en charge par Javascript.

En utilisant une boucle for et la fonction charAt() dans la deuxième méthode, nous pouvons boucler à travers chaque caractère de la chaîne d'origine et le remplacer par sa version en minuscules en utilisant également l'Unicode. Cette méthode peut être utile si vous avez besoin d'une personnalisation supplémentaire, par exemple en excluant certains caractères lors de la conversion vers les minuscules.

## Voir aussi

- [Documentation Javascript sur la fonction toLowerCase()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toLowerCase)
- [Documentation Javascript sur la fonction charAt()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/charAt)