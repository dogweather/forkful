---
date: 2024-01-20 17:52:55.237695-07:00
description: "L'affichage de messages de d\xE9bogage, c'est \xE9crire des infos dans\
  \ la console pour comprendre ce qui se passe dans le code. On le fait parce que\
  \ personne\u2026"
lastmod: 2024-02-19 22:05:16.923183
model: gpt-4-1106-preview
summary: "L'affichage de messages de d\xE9bogage, c'est \xE9crire des infos dans la\
  \ console pour comprendre ce qui se passe dans le code. On le fait parce que personne\u2026"
title: "Affichage des sorties de d\xE9bogage"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
L'affichage de messages de débogage, c'est écrire des infos dans la console pour comprendre ce qui se passe dans le code. On le fait parce que personne n'aime chercher une aiguille dans une botte de foin sans indice.

## How to (Comment faire ?)
Pour afficher des messages, utilisez `console.log()`. Pour des messages d'avertissement ou d'erreur, essayez `console.warn()` et `console.error()`.

```javascript
console.log("Salut, ça va ?");
console.warn("Attention, ça pourrait se compliquer...");
console.error("Oups, ça, c'était inattendu.");
```

Résultat :

```
Salut, ça va ?
Attention, ça pourrait se compliquer...
Oups, ça, c'était inattendu.
```

Bonus : `console.table()` affiche des données sous forme de tableau.

```javascript
console.table([{ nom: "Alice", points: 10 }, { nom: "Bob", points: 8 }]);
```

## Deep Dive (Plongée en profondeur)
Historiquement, `alert()` était utilisé, mais il interfère avec l'interaction utilisateur. Aujourd'hui, `console.log()` et ses variants sont standard. Chaque méthode a sa spécialité : `.log()` pour l'info générale, `.warn()` pour les avertissements, `.error()` pour les erreurs, et d'autres pour des besoins spécifiques comme `console.info()` ou `console.debug()`.

La console peut aussi afficher des objets et leurs propriétés, essentiel pour débugguer des structures complexes. 

Environnements comme Node.js et navigateurs modernes prennent en charge ces méthodes, qui n'affichent rien dans l'interface utilisateur mais dans un outil de développement séparé.

## See Also (Voir aussi)
- [MDN Web Docs: Console](https://developer.mozilla.org/fr/docs/Web/API/Console)
- [Console API Reference | Node.js](https://nodejs.org/api/console.html)
- [JavaScript.info: Debugging in the browser](https://javascript.info/debugging-chrome)
