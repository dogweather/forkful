---
date: 2024-01-26 00:53:51.728622-07:00
description: "La gestion des erreurs consiste \xE0 g\xE9rer les situations o\xF9 votre\
  \ code ne se comporte pas comme pr\xE9vu. C'est crucial car cela permet \xE0 vos\
  \ programmes de se\u2026"
lastmod: '2024-03-13T22:44:58.283689-06:00'
model: gpt-4-1106-preview
summary: "La gestion des erreurs consiste \xE0 g\xE9rer les situations o\xF9 votre\
  \ code ne se comporte pas comme pr\xE9vu. C'est crucial car cela permet \xE0 vos\
  \ programmes de se\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La gestion des erreurs consiste à gérer les situations où votre code ne se comporte pas comme prévu. C'est crucial car cela permet à vos programmes de se terminer en douceur et indique clairement aux utilisateurs ce qui se passe, au lieu de simplement planter.

## Comment faire :

Voici le classique bloc `try-catch` :

```javascript
try {
  // Code qui pourrait générer une erreur
  let result = potentiallyRiskyOperation();
  console.log('Succès :', result);
} catch (error) {
  // Que faire si une erreur est générée
  console.error('Oups :', error.message);
}
```

Exemple de sortie lorsqu'aucune erreur ne se produit :
```
Succès : 42
```

Et lorsqu'il y a une erreur :
```
Oups : Quelque chose s'est mal passé
```

Pour le code asynchrone, où les promesses sont impliquées, utilisez `try-catch` dans une fonction `async` :

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Données récupérées :', data);
  } catch (error) {
    console.error('Erreur lors de la récupération des données :', error.message);
  }
}

fetchData();
```

## Plongée en Profondeur

La gestion des erreurs en JavaScript a évolué. Dans le temps (ES3, vers 1999), nous avions tout juste le bloc `try-catch`. Pas super flexible, mais il faisait l'affaire.

ES6 (2015) a introduit les Promesses et nous a donné `.then()` et `.catch()`, nous permettant de gérer les erreurs asynchrones plus élégamment.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Données récupérées :', data))
  .catch(error => console.error('Erreur lors de la récupération des données :', error.message));
```

En ce qui concerne les détails de l'implémentation, lorsque une erreur est générée, les moteurs JavaScript créent un objet `Error` avec des propriétés utiles comme `message` et `stack`. Vous pouvez également créer des types d'erreurs personnalisés en étendant la classe `Error` – pratique pour des applications plus complexes.

Des alternatives ? Vous pourriez ignorer la gestion des erreurs (mauvaise idée), utiliser des callbacks avec des paramètres priorisant les erreurs (bonjour le style Node.js), ou vous sophisticquer avec des bibliothèques et des frameworks qui proposent leur propre gestion des erreurs.

## Voir Aussi

Pour en savoir plus sur la gestion des erreurs :

- MDN sur try-catch : [MDN try...catch](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await : [MDN fonction async](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Statements/async_function)
- Un guide sur les Promesses : [MDN Promises](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Créer et générer des erreurs personnalisées : [MDN Error](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Error)
