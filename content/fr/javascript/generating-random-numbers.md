---
title:    "Javascript: Génération de nombres aléatoires"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une compétence utile à avoir lorsqu'on fait de la programmation en Javascript. Cela peut servir à diverses fins, comme par exemple créer des jeux interactifs ou des simulations aléatoires.

## Comment faire

La méthode la plus courante pour générer des nombres aléatoires en Javascript est d'utiliser la fonction `Math.random()`. Cette fonction renvoie un nombre aléatoire compris entre 0 (inclus) et 1 (exclus). Voici un exemple de code qui utilise cette fonction pour générer 5 nombres aléatoires et les afficher dans la console :

```Javascript
for (let i = 0; i < 5; i++) {
  let randomNum = Math.random();
  console.log(randomNum);
}
```

Lorsque vous exécutez ce code, vous obtiendrez quelque chose comme ceci dans la console :

```
0.542367
0.009371
0.886413
0.738519
0.169473
```

Cependant, si vous souhaitez générer des nombres aléatoires dans une plage spécifique, vous pouvez utiliser la formule suivante :

```Javascript
Math.floor(Math.random() * (max - min + 1) + min);
```

Ici, `min` représente le nombre minimum que vous souhaitez inclure et `max` représente le nombre maximum que vous souhaitez inclure. Par exemple, si vous voulez générer un nombre aléatoire compris entre 1 et 10, vous pouvez utiliser cette formule :

```Javascript
let randomNum = Math.floor(Math.random() * (10 - 1 + 1) + 1);
console.log(randomNum); // affichera un nombre aléatoire compris entre 1 et 10
```

## Plongée en profondeur

La fonction `Math.random()` utilise un algorithme appelé "pseudo-aléatoire" pour générer des nombres. Cela signifie que les nombres ne sont pas vraiment aléatoires, mais qu'ils sont calculés à partir d'une certaine formule. Pour cette raison, si vous exécutez plusieurs fois le même code, vous obtiendrez la même séquence de nombres aléatoires.

Il est également important de noter que la méthode de génération de nombres aléatoires en Javascript peut varier selon les navigateurs et les versions de Javascript utilisées. Il est donc recommandé d'utiliser une bibliothèque ou un module spécifique si vous avez besoin de résultats vraiment aléatoires pour des applications sensibles.

## Voir aussi

- [Documentation sur la fonction Math.random() en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Math/random)
- [Bibliothèque pour générer des nombres aléatoires en Javascript](https://www.npmjs.com/package/random-js)
- [Article sur la génération de nombres aléatoires en Javascript](https://thecodingtrain.com/CodingChallenges/012-p5js-random.html)