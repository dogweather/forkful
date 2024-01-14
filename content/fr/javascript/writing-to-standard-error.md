---
title:                "Javascript: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers l'erreur standard est une pratique courante en programmation Javascript. Cela permet d'afficher des erreurs ou des messages de débogage dans la console du navigateur, ce qui peut aider les développeurs à identifier et à résoudre les problèmes dans leur code.

## Comment Faire

Il existe plusieurs façons d'écrire vers l'erreur standard en Javascript, mais la méthode la plus courante est d'utiliser la fonction `console.error()`. Cette fonction prend un ou plusieurs arguments et affiche leur contenu dans la console en tant qu'erreur.

```Javascript
console.error("Une erreur s'est produite.");
console.error("Voici un objet: " + { id: 1, nom: "John" });
```

Lorsque ces lignes de code sont exécutées, vous obtiendrez le résultat suivant dans la console:

```
Une erreur s'est produite.
Voici un objet: [object Object]
```

Comme vous pouvez le voir, la fonction `console.error()` peut prendre n'importe quel type de données en argument, y compris des chaînes de caractères, des nombres, des tableaux ou même des objets.

Une autre façon d'écrire vers l'erreur standard est d'utiliser `process.stderr.write()` si vous utilisez Node.js. Cette fonction permet également d'écrire des messages d'erreur dans la console.

```Javascript
process.stderr.write("Une erreur s'est produite.");
process.stderr.write("Voici un objet: " + { id: 1, nom: "John" });
```

Le résultat sera le même que lors de l'utilisation de `console.error()`.

## Deep Dive

La principale différence entre `console.error()` et `process.stderr.write()` est que la première utilise le gestionnaire de sortie du navigateur tandis que la seconde utilise le gestionnaire de sortie du système. Cela signifie que la fonction `console.error()` ne fonctionnera que dans le navigateur, tandis que `process.stderr.write()` ne fonctionnera que dans l'environnement Node.js.

De plus, `console.error()` peut également être utilisé pour afficher des avertissements en plus des erreurs, en utilisant la fonction `console.warn()`. Cela peut être utile pour différencier les types de messages dans la console.

## Voir Aussi

Pour en savoir plus sur l'écriture vers l'erreur standard en Javascript, voici quelques liens utiles:

- [Documentation officielle de console.error()](https://developer.mozilla.org/fr/docs/Web/API/Console/error)
- [Documentation officielle de process.stderr.write()](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_stderr)
- [Guide de débogage de Chrome pour console](https://developers.google.com/web/tools/chrome-devtools/console#write)