---
title:                "Javascript: Génération de nombres aléatoires"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes familier avec la programmation, vous savez probablement déjà que les nombres aléatoires sont un outil important dans la boîte à outils de tout programmeur. Mais pourquoi quelqu'un se donnerait-il la peine de générer des nombres aléatoires? En bref, c'est parce que les nombres aléatoires sont essentiels pour créer des simulations, des jeux, des algorithmes de chiffrement et bien plus encore.

## Comment Faire

La génération de nombres aléatoires en JavaScript est assez simple. Voici un exemple de code qui génère un nombre aléatoire entre 1 et 10.

```javascript
//Générer un nombre aléatoire entre 1 et 10
let randomNumber = Math.floor(Math.random() * 10) + 1;
```

Le résultat de cet exemple serait un nombre tel que 4, 8 ou 10. Voici une explication détaillée du code ci-dessus:

- La fonction `Math.random()` renvoie un nombre décimal aléatoire entre 0 et 1. Par exemple, il peut renvoyer 0,325 ou 0,876.
- La fonction `Math.floor()` arrondit le nombre décimal vers le bas pour obtenir un nombre entier. Ainsi, si `Math.random()` renvoie 0,325, `Math.floor()` renverra 0.
- En multipliant le résultat de `Math.random()` par 10, nous obtenons un nombre décimal entre 0 et 10, comme 3,25 ou 8,76.
- En utilisant `+ 1`, nous déplaçons les nombres vers le haut d'un cran, obtenant ainsi un nombre aléatoire entre 1 et 10.

Il est important de noter que la méthode ci-dessus n'est pas totalement aléatoire, mais elle est suffisamment proche pour la plupart des utilisations. Si vous avez besoin d'une solution plus aléatoire, il existe des bibliothèques tierces disponibles pour cela.

## Plongée Profonde

Maintenant que nous avons vu une méthode simple pour générer des nombres aléatoires en JavaScript, il est temps de plonger un peu plus profondément dans ce sujet. Tout d'abord, notons que la méthode que nous avons utilisée pour générer des nombres aléatoires produit des nombres pseudo-aléatoires. Cela signifie que les nombres ne sont pas vraiment aléatoires, mais qu'ils sont créés de manière déterministe à partir d'une "graine" initiale.

En JavaScript, cette "graine" est la date actuelle en millisecondes depuis le 1er janvier 1970. Cela signifie que si vous exécutez plusieurs fois le même code, vous obtiendrez le même résultat, car la date actuelle n'aura pas changé. Pour éviter cela, vous pouvez utiliser `Math.random()` comme graine pour générer des nombres plus aléatoires.

De plus, si vous avez besoin de générer des nombres aléatoires dans une plage spécifique, vous pouvez utiliser `Math.random()` en conjonction avec `Math.round()` ou `Math.ceil()`. Par exemple, si vous avez besoin d'un nombre aléatoire entre 25 et 75, vous pouvez utiliser le code suivant:

```javascript
let randomNumber = Math.round(Math.random() * 50) + 25;
```

Cela générera un nombre aléatoire entre 0 et 50, puis le déplacera vers le haut de 25, produisant ainsi un nombre entre 25 et 75.

## Voir Aussi

Voici quelques liens utiles pour en savoir plus sur la génération de nombres aléatoires en JavaScript:

- Document officiel de Mozilla sur `Math.random()`: https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Tutoriel de W3Schools sur les nombres aléatoires en JavaScript: https://www.w3schools.com/js/js_random.asp
- Bibliothèque pour générer des nombres aléatoires de haute qualité en JavaScript: https://github.com/davidbau/seedrandom