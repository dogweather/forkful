---
title:                "Génération de nombres aléatoires"
aliases:
- /fr/javascript/generating-random-numbers/
date:                  2024-01-27T20:34:14.825848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

La génération de nombres aléatoires en JavaScript est une technique utilisée pour créer de l'imprévisibilité dans les applications, depuis les jeux qui nécessitent un comportement ennemi aléatoire jusqu'aux algorithmes de sécurité nécessitant une aléatoire cryptographique. Cette capacité est cruciale pour développer des expériences utilisateur dynamiques et des applications sécurisées.

## Comment faire :

### Génération Basique de Nombres Aléatoires

La manière la plus simple de générer un nombre aléatoire en JavaScript est d'utiliser `Math.random()`. Cette fonction retourne un nombre à virgule flottante, pseudo-aléatoire dans l'intervalle 0 (inclusif) à 1 (exclusif).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Générer un Nombre Aléatoire dans un Intervalle

Souvent, vous voudrez un entier aléatoire dans un intervalle spécifique. Cela peut être réalisé en mettant à l'échelle et en arrondissant la sortie de `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Nombres Aléatoires Cryptographiquement Sécurisés

Pour les applications nécessitant un degré de hasard plus élevé (par exemple, les opérations cryptographiques), la méthode `crypto.getRandomValues()` peut être utilisée. Cela fournit une aléatoire cryptographique, contrairement aux nombres pseudo-aléatoires générés par `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Plongée en Profondeur

Historiquement, la génération de nombres aléatoires en JavaScript dépendait uniquement de la fonction `Math.random()`. Bien que pratique pour la plupart des cas d'usage occasionnels, son algorithme, typiquement une variante d'un générateur de nombres pseudo-aléatoires (PRNG) comme Mersenne Twister, ne fournissait pas de sécurité cryptographique.

L'introduction de l'API de Cryptographie Web a apporté la méthode `crypto.getRandomValues()`, offrant une manière de générer des nombres bien moins prévisibles et convenant aux applications sensibles à la sécurité. Cette méthode exploite les sources de hasard du système d'exploitation sous-jacent, telles que `/dev/random` sur Unix/Linux, qui sont plus robustes et appropriées pour les opérations cryptographiques.

Il est crucial de choisir la bonne méthode pour la tâche à accomplir. `Math.random()` suffit pour les besoins basiques comme les jeux simples, les animations, ou tout cas où la qualité du hasard n'est pas critique. Cependant, pour les fonctionnalités de sécurité, comme les jetons de réinitialisation de mot de passe ou toute opération cryptographique, `crypto.getRandomValues()` est le meilleur choix en raison de sa qualité de hasard supérieure.

Notamment, `Math.random()` génère des nombres avec un biais connu dans la plupart des implémentations, signifiant que certains nombres sont plus susceptibles de se produire que d'autres. Même si ce biais est minimal et souvent imperceptible pour les applications générales, il disqualifie `Math.random()` pour être utilisé dans tout contexte cryptographique ou applications où l'équité est essentielle, comme le jeu en ligne.

En conclusion, tandis que les fonctions intégrées de JavaScript pour générer des nombres aléatoires couvrent une large gamme de besoins, comprendre les différences et les limitations de chaque méthode est essentiel pour leur application appropriée.
