---
title:    "TypeScript: La génération de nombres aléatoires"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler être une tâche inutile pour certains, mais il est en fait très utile dans de nombreux scénarios de programmation. Que vous ayez besoin de créer une loterie virtuelle, de simuler des données aléatoires pour des tests ou de générer des mots de passe aléatoires, la génération de nombres aléatoires peut être un outil précieux à avoir dans votre boîte à outils de programmation.

## Comment Faire

Pour générer des nombres aléatoires en TypeScript, nous allons utiliser la fonction `Math.random()`. Cette fonction génère un nombre aléatoire compris entre 0 et 1. Pour obtenir un nombre aléatoire dans une plage spécifique, disons entre 1 et 100, nous pouvons utiliser `Math.random() * 100 + 1` comme suit:

```TypeScript
let randomNumber = Math.random() * 100 + 1;
console.log(randomNumber);
```

Cela imprimera un nombre aléatoire compris entre 1 et 100 dans la console chaque fois que le code sera exécuté.

Nous pouvons également utiliser la fonction `Math.floor()`, qui arrondit un nombre à l'entier inférieur, pour obtenir un nombre entier aléatoire dans notre plage souhaitée. Nous pouvons combiner cela avec `Math.random()` pour créer une fonction réutilisable qui génère un nombre entier aléatoire entre une valeur minimale et maximale spécifique:

```TypeScript
function getRandomNumber(min: number, max: number) : number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

let randomNumber = getRandomNumber(1, 100);
console.log(randomNumber); // imprime un nombre aléatoire entre 1 et 100 à chaque exécution
```

## Plongée Profonde

La fonction `Math.random()` utilise un algorithme de génération de nombres pseudo-aléatoires, ce qui signifie que les nombres générés peuvent en fait être prédits. Si vous avez besoin de nombres vraiment aléatoires, vous devriez envisager d'utiliser un générateur de nombres aléatoires cryptographiquement sécurisé (CSPRNG).

Pour cela, TypeScript fournit la classe `crypto` qui comprend plusieurs méthodes pour générer des nombres aléatoires cryptographiquement sécurisés. Par exemple, `crypto.getRandomValues()` peut être utilisé pour générer un tableau de nombres aléatoires de taille spécifique. Mais gardez à l'esprit que ces méthodes peuvent être assez coûteuses en termes de performance, donc ils ne devraient être utilisés que si la sécurité est une préoccupation majeure.

## Voir Aussi

- Documentation officielle sur `Math.random()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Math/random
- Explications sur les générateurs de nombres aléatoires cryptographiquement sécurisés en JavaScript: https://blog.bitsrc.io/random-numbers-in-javascript-2c4ecff6e58b