---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire des tests, c'est comme mettre un filet de sécurité sur votre code pour chasser les bugs. Les développeurs le font pour dormir tranquilles, en sachant que leur code fonctionne comme prévu et que les nouvelles modifications ne cassent rien.

## Comment faire :

Voici un test unitaire simple avec Jest, un framework populaire.

```TypeScript
import { somme } from './math';

test('additionne 2 + 2 pour obtenir 4', () => {
  expect(somme(2, 2)).toBe(4);
});
```

Si tout va bien, le résultat affiché sera :

```
PASS  ./math.test.ts
✓ additionne 2 + 2 pour obtenir 4 (5ms)
```

## Plongée Profonde

Historiquement, les tests sont anciens comme le code. Mais ce n'est que récemment que les frameworks de tests modernes ont simplifié la tâche. Alternatives ? Il y en a plein : Mocha, Jasmine, et Ava, pour n'en nommer que quelques-uns. Pour l'implémentation ? On écrit souvent des tests en employant le TDD (Test-Driven Development) – où les tests guident le design du code.

## Voir Aussi

* Jest: [https://jestjs.io/fr/](https://jestjs.io/fr/)
* Jasmine: [https://jasmine.github.io/](https://jasmine.github.io/)
* Mocha: [https://mochajs.org/](https://mochajs.org/)
* Guide TDD: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
