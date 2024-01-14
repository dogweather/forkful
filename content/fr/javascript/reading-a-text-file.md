---
title:                "Javascript: Lecture d'un fichier texte"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire et traiter des fichiers texte sont des tâches courantes pour les programmeurs Javascript. Que vous ayez besoin d'analyser des données, de manipuler des fichiers ou de créer des scripts de traitement de texte, savoir lire un fichier texte est une compétence essentielle pour tout développeur.

## Comment faire

Pour lire un fichier texte en Javascript, il existe deux méthodes principales : à l'aide du module "fs" et du module "readline". Voici un exemple de code utilisant le module "fs" pour lire un fichier texte :

```Javascript
const fs = require('fs');
// Lecture d'un fichier texte en utilisant fs.readFile()
fs.readFile('monfichier.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

La méthode "fs.readFile()" prend trois paramètres : le nom du fichier à lire, l'encodage du fichier et une fonction de rappel qui se déclenchera une fois le fichier lu. Dans cet exemple, nous utilisons l'encodage "utf8" pour lire le fichier en tant que texte et la fonction de rappel affiche les données dans la console. Vous pouvez également utiliser la méthode "fs.readFileSync()" pour lire un fichier de manière synchronisée, mais cela peut bloquer l'exécution de votre programme si le fichier est très volumineux.

En utilisant le module "readline", vous pouvez lire un fichier texte ligne par ligne. Voici un exemple de code utilisant le module "readline" pour lire un fichier texte :

```Javascript
const readline = require('readline');
// Création d'une interface de lecture
const rl = readline.createInterface({
  input: fs.createReadStream('monfichier.txt'),
  crlfDelay: Infinity
});
// Parcourir chaque ligne du fichier
rl.on('line', (line) => {
  console.log(`Ligne lue : ${line}`);
});
```

En utilisant cette méthode, chaque ligne du fichier sera traitée une à une dans la fonction de rappel. Vous pouvez également utiliser le module "readline-sync" pour lire un fichier de manière synchrone.

## Approfondissement

Lorsque vous lisez un fichier texte en Javascript, il est important de comprendre comment les données sont structurées dans le fichier. Vous pouvez utiliser des expressions régulières pour extraire des informations spécifiques d'un fichier texte, ou utiliser des méthodes de manipulation de chaînes comme "split()" et "slice()" pour découper les données en différents éléments. Il est également important de gérer les erreurs lors de la lecture d'un fichier, en utilisant un bloc "try...catch" pour gérer les exceptions.

## Voir aussi
- [Documentation sur le module "fs"](https://nodejs.org/api/fs.html)
- [Documentation sur le module "readline"](https://nodejs.org/api/readline.html)
- [Documentation sur le module "readline-sync"](https://github.com/anseki/readline-sync)