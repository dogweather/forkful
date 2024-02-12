---
title:                "Rédaction de tests"
aliases:
- /fr/typescript/writing-tests.md
date:                  2024-02-03T19:32:13.162701-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rédaction de tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire des tests en TypeScript implique de créer des scripts automatisés pour vérifier la fonctionnalité et la correction de votre code. Les programmeurs le font pour assurer la fiabilité, attraper rapidement les bugs, et faciliter une croissance de code maintenable, étant donné que le typage statique de TypeScript ajoute un niveau de prévisibilité aux tests JavaScript.

## Comment faire :
TypeScript fonctionne harmonieusement avec la plupart des cadres de test JavaScript. À titre de démonstration, nous utiliserons Jest, un cadre de test populaire, en raison de sa configuration zéro pour les projets TypeScript.

Premièrement, assurez-vous d'avoir Jest et les types TypeScript nécessaires installés :

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Ensuite, configurez Jest pour travailler avec TypeScript en modifiant le `jest.config.js` ou en en créant un nouveau :

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Maintenant, écrivons une fonction simple et un test pour elle. Considérez un fichier `sum.ts` avec la fonction suivante :

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Créez un fichier de test nommé `sum.test.ts` :

```typescript
// sum.test.ts
import { sum } from './sum';

test('additionne 1 + 2 pour égaler 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Exécutez vos tests avec :

```bash
npx jest
```

Un exemple de sortie indiquant un test réussi devrait ressembler à ceci :

```plaintext
 PASS  ./sum.test.ts
  ✓ additionne 1 + 2 pour égaler 3 (2 ms)
```

Pour du code asynchrone, Jest s'adapte avec `async/await`. Supposons que vous ayez une fonction asynchrone `fetchData` :

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Votre test utilisant des fonctions asynchrones :

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('récupère les données avec succès', async () => {
  expect(await fetchData()).toBe('data');
});
```

Lors de l'exécution de vos tests, Jest attendra que la promesse soit résolue, testant correctement les opérations asynchrones.

Rappelez-vous, un test efficace inclut l'écriture de plusieurs tests pour différents scénarios, y compris les cas limites, pour s'assurer que votre code TypeScript se comporte comme prévu.
