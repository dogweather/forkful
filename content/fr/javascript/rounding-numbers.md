---
title:                "Arrondir les nombres"
date:                  2024-01-26T03:45:22.383256-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrondir les nombres"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'arrondi consiste à couper le bruit après un certain point dans un nombre. Les programmeurs arrondissent pour contrôler la précision, gérer la mémoire, ou rendre la sortie plus conviviale — comme transformer 2.998 en un net 3.

## Comment faire :
Voici comment vous arrondissez les nombres en JavaScript en utilisant `Math.round()`, `Math.ceil()`, et `Math.floor()` :

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (puisque .567 est plus que .5)

console.log(roundedDown); // Imprime : 2
console.log(roundedUp);   // Imprime : 3
console.log(rounded);     // Imprime : 3
```

Pour fixer un certain nombre de décimales, utilisez `toFixed()` :

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (retourne une chaîne de caractères)

console.log(twoDecimals); // Imprime : "2.57"
```

Convertissez la chaîne de retour en un nombre avec un plus unaire ou `Number()` :

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Imprime : 2.57
```

## Approfondissement
Arrondir les nombres n'est pas nouveau ; cela existe depuis aussi longtemps que les nombres. En JavaScript, `Math.round()` utilise le "round half up" pour la règle de bris d'égalité : si la partie fractionnaire est 0.5, il arrondit au nombre pair le plus proche.

Pour plus de contrôle, `toFixed()` pourrait être votre prédilection, mais souvenez-vous, il retourne une chaîne. Le reconvertir en nombre pourrait être une étape supplémentaire mais assure que vous continuez à travailler avec des types numériques.

Des alternatives ? Des bibliothèques comme `lodash` offrent `_.round(number, [precision=0])` pour un contrôle plus nuancé. Ou, la plus récente `Intl.NumberFormat` vous donne un formatage de haute précision au-delà de l'arrondissement.

Parlant de précision, méfiez-vous des bizarreries de point flottant en JavaScript. `0.1 + 0.2` n'équivaut pas exactement à `0.3` en raison de la manière dont les nombres sont stockés. Parfois, l'arrondissement devient nécessaire pour corriger de telles erreurs de point flottant.

## Voir Aussi
- Documentation Math de Mozilla : [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Arrondissement financier avec `Intl.NumberFormat` : [API d'internationalisation ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Arrondissement avec `lodash` : [Docs de Lodash](https://lodash.com/docs/4.17.15#round)
