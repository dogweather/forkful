---
title:                "Écrire des tests"
html_title:           "TypeScript: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?

Écrire des tests est une pratique courante chez les programmeurs. Il s'agit de créer du code spécifiquement pour tester votre code existant et s'assurer qu'il fonctionne correctement. Cela permet de détecter les erreurs et de s'assurer que votre programme est de qualité.

## Comment faire :

Voici un exemple en TypeScript d'un test de fonction pour calculer la moyenne d'un tableau de nombres :

```TypeScript
function calculerMoyenne(nombres: number[]): number {
  let somme: number = 0;
  for (let nombre of nombres) {
    somme += nombre;
  }
  return somme / nombres.length;
}

// Test
describe("calculerMoyenne", () => {
  it("devrait retourner la moyenne d'un tableau de nombres", () => {
    const result = calculerMoyenne([1, 2, 3, 4, 5]);
    expect(result).toBe(3);
  });
});
```

Lorsque nous exécutons ce test, il vérifie si la fonction calculerMoyenne renvoie bien la moyenne correcte pour le tableau de nombres donné. Si le test échoue, cela signifie qu'il y a un problème dans notre fonction et nous devons le corriger.

## Approfondissement :

La pratique de l'écriture de tests, également connue sous le nom de développement piloté par les tests (Test-driven development en anglais), a été popularisée dans les années 2000 par le programmeur Kent Beck. Elle est basée sur l'idée que les tests peuvent servir de spécifications pour votre code, en vous forçant à réfléchir à toutes les possibilités et cas de bord avant d'écrire votre code.

Il existe également d'autres façons d'effectuer des tests, telles que le développement piloté par les comportements (Behaviour-driven development en anglais) ou les tests unitaires. Chacune a ses avantages et ses inconvénients, et il est important de choisir celle qui convient le mieux à votre projet.

L'implémentation des tests en TypeScript est relativement simple, car il dispose d'un framework de tests intégré appelé Jest, qui fournit des fonctionnalités telles que describe et it pour structurer vos tests et expect pour effectuer des assertions.

## Voir aussi :

- [Documentation officielle de Jest](https://jestjs.io/docs/en/getting-started)
- [Introduction au développement piloté par les tests en JavaScript](https://www.agilealliance.org/glossary/tdd/)
- [Dix règles pour le développement piloté par les tests](https://pragprog.com/magazines/2012-01/pragpub/january-2012-tdd)