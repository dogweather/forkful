---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests, c'est comme faire une check-list pour ton code. Ça aide à s'assurer que tout fonctionne nickel, même après des modifs. T'as moins de bugs et tu dors sur tes deux oreilles.

## How to:
Pour tester, on utilise des cadriciels (frameworks) comme Jest. Voici un exemple de test:

```Javascript
// fonction à tester
function ajouteDeux(a) {
  return a + 2;
}

// Test avec Jest
test('ajouteDeux ajoute 2 à n’importe quel nombre', () => {
  expect(ajouteDeux(3)).toBe(5);
});
```

Si t'as bien fait le job, la console te dira:

```
PASS  ./example.test.js
✓ ajouteDeux ajoute 2 à n’importe quel nombre (5 ms)
```

## Deep Dive
Back in the day, on testait à la mano en checkant le code ligne par ligne. Aujourd'hui, on a des outils automatiques. Jest est populaire car il est simple, mais t'as aussi Mocha, Jasmine ou bien d'autres. Choisir c'est renoncer - chaque outil a ses forces, ses faiblesses et sa façon de faire.

Pour écrire des tests qui tiennent la route, pense SOLID et DRY. Teste aussi les cas limites - c'est là que les bugs se planquent.

## See Also
Va jeter un œil ici pour plus d'info :
- Jest: [https://jestjs.io/fr/](https://jestjs.io/fr/)
- Mocha: [https://mochajs.org/](https://mochajs.org/)
- Jasmine: [https://jasmine.github.io/](https://jasmine.github.io/)
