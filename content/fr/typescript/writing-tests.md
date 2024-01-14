---
title:                "TypeScript: Ecriture de tests"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire des tests est un élément essentiel du processus de développement logiciel. Cela permet de valider le bon fonctionnement de votre code et de détecter les erreurs avant qu'elles ne deviennent des problèmes majeurs. Les tests rendent également le processus de débogage plus facile en ciblant spécifiquement les parties du code qui présentent des problèmes.

## Comment faire

Pour commencer à écrire des tests en TypeScript, il suffit d'utiliser le framework de test intégré appelé "Jest". Jest est facile à configurer et à utiliser, et il vous permet de tester votre code avec des assertions simples et intuitives.

Voici un exemple de code TypeScript où nous testons une fonction simple qui ajoute deux nombres et renvoie le résultat :

```typescript
function sum(a: number, b: number) {
    return a + b;
}

test('La fonction sum devrait retourner la somme des deux nombres', () => {
    expect(sum(2, 3)).toBe(5);
});
```

Dans cet exemple, nous avons créé un test en utilisant la fonction `test` de Jest et avons utilisé l'assertion `expect` pour vérifier que notre fonction `sum` renvoie le résultat attendu. Jest prend en charge une variété d'assertions telles que `toBe` pour vérifier l'égalité stricte, `toEqual` pour vérifier l'égalité de valeurs et bien d'autres.

## Plongée en profondeur

Écrire des tests efficaces en TypeScript implique de bien comprendre le concept de types. Il est important de tester tous les scénarios possibles, y compris ceux où les variables peuvent avoir des valeurs null ou undefined. Les blocs de tests avec des itérations sur des tableaux ou des objets peuvent également être utiles pour couvrir tous les cas possibles.

Il est également important d'utiliser des outils de couverture de code tels que Istanbul pour mesurer la couverture de vos tests et vous assurer que toutes les parties de votre code sont testées correctement.

## Voir aussi

- [Documentation Jest](https://jestjs.io/fr/)
- [Les tests unitaires en TypeScript pour les débutants](https://www.technopedia.fr/les-tests-unitaires-en-typescript-pour-les-debutants/)