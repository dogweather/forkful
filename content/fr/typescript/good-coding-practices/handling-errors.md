---
date: 2024-01-26 00:57:59.710834-07:00
description: "G\xE9rer les erreurs consiste \xE0 anticiper l'inattendu ; c'est la\
  \ fa\xE7on dont nous r\xE9agissons lorsque les choses tournent mal dans notre code.\
  \ Nous le faisons\u2026"
lastmod: 2024-02-19 22:05:16.274738
model: gpt-4-1106-preview
summary: "G\xE9rer les erreurs consiste \xE0 anticiper l'inattendu ; c'est la fa\xE7\
  on dont nous r\xE9agissons lorsque les choses tournent mal dans notre code. Nous\
  \ le faisons\u2026"
title: Gestion des erreurs
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Gérer les erreurs consiste à anticiper l'inattendu ; c'est la façon dont nous réagissons lorsque les choses tournent mal dans notre code. Nous le faisons pour éviter les plantages et pour offrir aux utilisateurs une expérience fluide, même lorsque l'inattendu se produit.

## Comment faire :
En TypeScript, la gestion des erreurs implique souvent l'utilisation des blocs `try`, `catch` et `finally`.

```typescript
function operationRisquee() {
  throw new Error("Quelque chose a mal tourné !");
}

function gererLesErreurs() {
  try {
    operationRisquee();
  } catch (erreur) {
    console.error("Erreur capturée :", erreur.message);
  } finally {
    console.log("Ceci s'exécute toujours, qu'il y ait une erreur ou non.");
  }
}

gererLesErreurs();
```

Sortie de l'exemple :

```
Erreur capturée : Quelque chose a mal tourné !
Ceci s'exécute toujours, qu'il y ait une erreur ou non.
```

Exemple asynchrone avec des promesses :

```typescript
async function operationAsynchroneRisquee() {
  return new Promise((resolve, reject) => {
    // Simuler une erreur
    reject("Échec lamentable");
  });
}

async function gererLesErreursAsynchrones() {
  try {
    await operationAsynchroneRisquee();
  } catch (erreur) {
    console.error("Erreur asynchrone capturée :", erreur);
  }
}

gererLesErreursAsynchrones();
```

Sortie de l'exemple :

```
Erreur asynchrone capturée : Échec lamentable
```

## Plongée en profondeur
La gestion des erreurs est un pilier de la programmation depuis ses débuts. En TypeScript, qui s'appuie sur JavaScript, la gestion des erreurs est devenue plus robuste avec l'introduction de async/await en ECMAScript 2017. Avant cela, nous comptions souvent sur des fonctions de rappel (callbacks) et des promesses pour gérer les erreurs dans le code asynchrone.

Une alternative à `try/catch` en TypeScript est d'utiliser des limites d'erreurs fournies par des frameworks tels que React. Pour la gestion côté serveur, nous pouvons utiliser des logiciels intermédiaires (middleware) sur des plateformes comme Express.js pour centraliser la gestion des erreurs.

En termes d'implémentation, TypeScript n'a pas de mécanisme de gestion des erreurs propre, mais repose sur celui de JavaScript. Des classes d'erreurs personnalisées peuvent étendre la classe `Error` pour offrir des informations d'erreur plus descriptives.

## Voir aussi
- [MDN sur try/catch](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Instructions/try...catch)
- [Async/Await sur MDN](https://developer.mozilla.org/fr/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Utilisation des limites d'erreurs dans React](https://fr.reactjs.org/docs/error-boundaries.html)
- [Gestion des erreurs dans Express.js](https://expressjs.com/fr/guide/error-handling.html)
