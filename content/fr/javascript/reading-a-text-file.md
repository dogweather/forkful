---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Ça alors! Quoi & Pourquoi? 

Lire un fichier texte, c'est extraire des données enregistrées dans un fichier en tant que texte. Pourquoi les programmeurs font-ils ça? C'est simple. Ils veulent manipuler, analyser, ou afficher ces données dans leurs applications.

## Comment faire :

Nous allons utiliser le module de base `fs` (fichier système) de Node.js. Voici un exemple simple pour ouvrir et lire un fichier texte en JavaScript.

```Javascript
// Charger le module fs
var fs = require('fs');

// Lecture du fichier
fs.readFile('MonFichier.txt', 'utf8', function(err, data){
  if (err) throw err;
  console.log(data);
});
```
Imaginez que `MonFichier.txt` contient le texte "Bonjour à tous!". L'exécution de ce programme donnera l'affichage suivant.

```Javascript
Bonjour à tous!
```

## Plongeons dans le sujet

Historiquement, la lecture de fichiers texte est une technique courante utilisée depuis les premiers jours de la programmation pour stocker et récupérer des données.

En termes d'alternatives, vous pourriez utiliser d'autres modules comme `fs-extra` qui ajoutent quelques fonctionnalités utiles à `fs`. Ou si vous travaillez avec JSON, vous pouvez utiliser `require` pour lire directement des fichiers JSON.


```Javascript
var data = require('./MonFichier.json');
console.log(data);
```

Dans notre exemple sur `fs`, vous remarquerez qu'on a utilisé une fonction de rappel (callback) pour gérer les données après la lecture du fichier texte. C'est important car Node.js opère de manière asynchrone. Pour une approche plus moderne, vous pouvez utiliser les Promesses ou async/await pour améliorer davantage la lisibilité et la manipulation des erreurs.

## À voir aussi 

- Documentation officielle de Node.js sur [fs](https://nodejs.org/api/fs.html)
- Un tutoriel sur les [Promesses](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Utiliser_les_promesses)
- NPM module: [`fs-extra`](https://www.npmjs.com/package/fs-extra)