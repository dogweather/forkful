---
title:                "Javascript: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi générer des nombres aléatoires en Javascript ?

Si vous êtes un programmeur en herbe ou expérimenté, vous savez sûrement combien il est important de pouvoir générer des nombres aléatoires dans un programme. Que ce soit pour créer des algorithmes d'IA, des jeux, ou simplement pour ajouter un élément de surprise dans votre application, la génération de nombres aléatoires est un outil essentiel. Dans cet article, nous allons vous montrer comment le faire en Javascript.

## Comment le faire ?

```Javascript
// Pour générer un nombre aléatoire entre 0 et 10 :
Math.floor(Math.random() * 10);
// Output: un nombre entier aléatoire entre 0 et 10 (exclu)

// Pour générer un nombre aléatoire entre 1 et 10 :
Math.floor(Math.random() * 10) + 1;
// Output: un nombre entier aléatoire entre 1 et 10

// Pour générer un nombre à virgule aléatoire entre 0 et 1 :
Math.random();
// Output: un nombre décimal aléatoire entre 0 (inclus) et 1 (exclu)
```

Pour générer des nombres aléatoires en Javascript, nous utilisons la méthode `Math.random()`, qui renvoie un nombre décimal aléatoire entre 0 et 1 (0 inclus, 1 exclu). Pour obtenir un résultat entier, il faut utiliser la méthode `Math.floor()` qui arrondit le nombre à l'entier inférieur. En multipliant le résultat de `Math.random()` par le nombre de valeurs désirées et en y ajoutant éventuellement un nombre, nous pouvons obtenir des nombres aléatoires dans la plage souhaitée.

Pour un meilleur contrôle sur les nombres aléatoires générés, vous pouvez utiliser la librairie JavaScript `Chance.js` qui offre de nombreuses fonctionnalités pour la génération de nombres, de phrases, de dates, etc.

## Approfondir le sujet

Il est important de comprendre que les nombres aléatoires générés par Javascript ne sont pas vraiment aléatoires car ils sont basés sur un algorithme pseudo-aléatoire. Cela signifie que si vous utilisez la même méthode, vous obtiendrez la même séquence de nombres aléatoires après chaque exécution du programme. Cela peut être utile pour la débogage, mais si vous avez besoin de vrais nombres aléatoires, il est recommandé d'utiliser une librairie externe.

De plus, il est important de se rappeler que la génération de nombres aléatoires peut provoquer des bugs dans votre code, car vous ne pouvez pas contrôler leur valeur. Soyez donc prudent lorsque vous utilisez des nombres aléatoires dans votre application et testez-les soigneusement pour éviter tout problème.

## Voir aussi

- [Documentation officielle de Math.random()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Math/random)
- [La librairie "Chance.js"](https://chancejs.com/) pour une génération plus avancée de nombres aléatoires en Javascript.