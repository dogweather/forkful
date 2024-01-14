---
title:    "Javascript: Écriture de tests"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en Javascript?

Lorsque vous écrivez du code en Javascript, il est important de s'assurer que celui-ci fonctionne correctement et de manière cohérente. Les tests automatisés sont un moyen efficace de garantir la fiabilité de votre code et de détecter d'éventuels bugs.

## Comment écrire des tests en Javascript?

Pour commencer, vous devez choisir un outil de test adapté à votre projet. L'un des plus populaires est Jest, qui offre de nombreuses fonctionnalités pratiques telles que le mocking et l'assertion de valeurs. Voici un exemple de code pour tester une fonction de calcul de moyenne :

```Javascript
// Fonction à tester
function calculerMoyenne(notes) {
  let somme = 0;

  for (let i = 0; i < notes.length; i++) {
    somme += notes[i];
  }

  return somme / notes.length;
}

// Tests
test('calculerMoyenne retourne la moyenne correcte', () => {
  expect(calculerMoyenne([16, 18, 20])).toBe(18);
  expect(calculerMoyenne([10, 8, 12, 14])).toBe(11);
});
```

En utilisant Jest, nous pouvons vérifier que notre fonction `calculerMoyenne` renvoie la moyenne correcte pour différents ensembles de notes. Le code peut sembler un peu intimidant au début, mais une fois que vous avez compris les principes de base, écrire des tests devient beaucoup plus facile.

## Approfondissement sur l'écriture de tests en Javascript

Au delà des exemples simples, il existe une multitude de techniques et de bonnes pratiques pour écrire des tests en Javascript. Par exemple, l'utilisation du TDD (Test-Driven Development) où l'on écrit d'abord les tests avant le code, permet de réfléchir à l'architecture de son programme en amont et de détecter les erreurs plus rapidement. Il est également important de comprendre comment bien structurer ses tests et de savoir lesquels ont le plus de valeur dans la détection d'éventuels bugs.

## Voir aussi
- [Documentation officielle de Jest](https://jestjs.io/)
- [Guide complet pour écrire des tests en Javascript](https://medium.freecodecamp.org/a-complete-guide-to-writing-tests-for-your-javascript-code-using-jest-dd01495979c6)
- [TDD pour les débutants](https://www.freecodecamp.org/news/learning-tdd-in-javascript-tutorial-lessons-specifically-for-the-freecodecamp-curriculum-5350ae762e49/)