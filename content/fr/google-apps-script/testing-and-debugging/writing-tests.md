---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:34.498508-07:00
description: "Comment faire : Bien que Google Apps Script n'ait pas de cadre de test\
  \ int\xE9gr\xE9 comme certains autres environnements de programmation, vous pouvez\
  \ toujours\u2026"
lastmod: '2024-03-13T22:44:57.190207-06:00'
model: gpt-4-0125-preview
summary: "Bien que Google Apps Script n'ait pas de cadre de test int\xE9gr\xE9 comme\
  \ certains autres environnements de programmation, vous pouvez toujours \xE9crire\
  \ et ex\xE9cuter des tests en utilisant des fonctions GAS simples ou en int\xE9\
  grant des biblioth\xE8ques de tests externes telles que `QUnit`."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
Bien que Google Apps Script n'ait pas de cadre de test intégré comme certains autres environnements de programmation, vous pouvez toujours écrire et exécuter des tests en utilisant des fonctions GAS simples ou en intégrant des bibliothèques de tests externes telles que `QUnit`. Voici un exemple de base utilisant une simple fonction GAS pour tester une autre fonction de votre script :

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Test échoué : add(2, 3) devrait être 5, mais était " + result);
  } else {
    Logger.log("Test réussi !");
  }
}
```

Exécuter `testAdd()` inscrira "Test réussi !" si la fonction `add` fonctionne correctement, ou lancera une erreur si ce n'est pas le cas. Pour une approche plus sophistiquée, intégrer QUnit avec Google Apps Script implique quelques étapes supplémentaires mais offre un environnement de test puissant. Un exemple de configuration de test QUnit ressemble à ceci :

1. Inclure la bibliothèque QUnit dans votre projet.
2. Créer un fichier HTML de test pour exécuter les tests QUnit.
3. Écrire des cas de test en utilisant la syntaxe de QUnit.

Voici un exemple utilisant QUnit :

```javascript
// Inclure QUnit en le liant dans un fichier HTML utilisé pour exécuter vos tests

QUnit.test("Tester la fonction add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) devrait retourner 5");
});
```

Pour voir les résultats, ouvrez le fichier HTML dans l'éditeur de script GAS ou déployez-le en tant qu'application web.

## Plongée en profondeur
Historiquement, les tests dans Google Apps Script ont été quelque peu négligés, probablement en raison des origines de la plateforme et des cas d'utilisation principaux qui se concentrent sur des tâches d'automatisation rapides et à petite échelle plutôt que sur de grandes applications. En tant que tel, GAS n'offre pas les mêmes cadres et outils de test robustes que l'on trouve dans des environnements de programmation plus traditionnels. Cependant, la communauté s'est adaptée en incorporant des bibliothèques open-source et en utilisant de manière créative les outils existants de Google.

Utiliser des bibliothèques comme QUnit représente un grand pas en avant mais vient avec son propre ensemble de défis, tels que la mise en place d'un environnement de test adapté et l'apprentissage d'une syntaxe supplémentaire. Cependant, pour ceux qui sont investis dans la construction d'applications plus complexes et fiables avec GAS, l'effort en vaut la peine.

Les alternatives comme l'utilisation de fonctions GAS simples pour les tests offrent une facilité d'utilisation et une intégration avec l'environnement GAS sans dépendances supplémentaires, mais manquent de fonctionnalités de test complètes et de la capacité à évoluer facilement à mesure que votre projet grandit. Des outils tels que clasp (l'interface de ligne de commande Google Apps Script) peuvent faciliter des flux de travail plus avancés, y compris les tests, en permettant aux développeurs de coder dans leur IDE préféré, introduisant ainsi la possibilité d'intégrer plus aisément des cadres de test externes.

En conclusion, bien que GAS n'ait pas de support natif pour les tests sophistiqués dès le départ, sa flexibilité et les approches innovantes de la communauté fournissent des voies viables pour garantir que vos scripts sont robustes, fiables et prêts pour toute tâche.
