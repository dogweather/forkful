---
date: 2024-01-27 20:35:26.967318-07:00
description: "La g\xE9n\xE9ration de nombres al\xE9atoires en TypeScript consiste\
  \ \xE0 cr\xE9er des valeurs num\xE9riques impr\xE9visibles dans une plage sp\xE9\
  cifi\xE9e. Les programmeurs\u2026"
lastmod: '2024-03-11T00:14:31.448488-06:00'
model: gpt-4-0125-preview
summary: "La g\xE9n\xE9ration de nombres al\xE9atoires en TypeScript consiste \xE0\
  \ cr\xE9er des valeurs num\xE9riques impr\xE9visibles dans une plage sp\xE9cifi\xE9\
  e. Les programmeurs\u2026"
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La génération de nombres aléatoires en TypeScript consiste à créer des valeurs numériques imprévisibles dans une plage spécifiée. Les programmeurs utilisent ces chiffres aléatoires pour une variété de buts, tels que la génération d'identifiants uniques, la simulation de données pour les tests ou l'ajout d'imprévisibilité aux jeux et simulations.

## Comment :

En TypeScript, vous pouvez générer des nombres aléatoires en utilisant l'objet global `Math`. Voici quelques exemples pratiques démontrant comment produire des nombres aléatoires pour différents besoins.

### Générer un Nombre Aléatoire de Base

Pour générer un nombre décimal aléatoire de base entre 0 (inclus) et 1 (exclus), vous utilisez `Math.random()`. Cela ne nécessite aucune manipulation supplémentaire :

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

Cela pourrait afficher une valeur comme `0.8995452185604771`.

### Générer un Entier Aléatoire Entre Deux Valeurs

Lorsque vous avez besoin d'un entier entre deux valeurs spécifiques, vous incorporez à la fois `Math.random()` et un peu d'arithmétique :

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

Cela pourrait afficher une valeur entière entre 1 et 10, comme `7`.

### Générer un Identifiant Unique

Les nombres aléatoires peuvent être combinés avec d'autres méthodes pour créer des identifiants uniques, par exemple, un extrait de générateur d'UUID simple :

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

Cela génère une chaîne ressemblant à un UUID, comme `110e8400-e29b-41d4-a716-446655440000`.

## Approfondissement

La méthode principale pour générer des nombres aléatoires en JavaScript et donc en TypeScript, `Math.random()`, repose sur un générateur de nombres pseudo-aléatoires (PRNG). Il est important de noter que, bien que les résultats puissent sembler aléatoires, ils sont générés par un algorithme déterministe basé sur une valeur de graine initiale. Par conséquent, les nombres produits par `Math.random()` ne sont pas vraiment aléatoires et ne devraient pas être utilisés à des fins cryptographiques.

Pour des nombres aléatoires cryptographiquement sécurisés, l'API Web Crypto propose `crypto.getRandomValues()`, accessible dans les environnements supportant la norme Web Crypto, y compris les navigateurs modernes et Node.js (via le module `crypto`). Voici un rapide exemple illustrant son utilisation en TypeScript pour générer un nombre aléatoire sécurisé dans une plage :

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Cette méthode offre un niveau de hasard plus élevé et est plus adaptée pour des applications sensibles à la sécurité. Cependant, elle est également plus gourmande en ressources et peut ne pas être nécessaire pour des tâches plus banales, comme des simulations simples ou la génération de valeurs aléatoires non critiques.
