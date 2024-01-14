---
title:                "Javascript: Lecture des arguments en ligne de commande"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande peuvent sembler intimidants au premier abord, mais ils sont en fait un outil utile pour les développeurs Javascript. En lisant cet article, vous découvrirez comment les utiliser pour améliorer votre expérience de programmation.

## Comment faire

Pour lire les arguments de ligne de commande en utilisant Javascript, vous pouvez utiliser l'objet `process.argv` inclus dans Node.js. Ce tableau contient tous les arguments passés lors de l'exécution de votre programme.

Voici un exemple de code:

```Javascript
// Récupère le premier argument passé lors de l'exécution
let argument = process.argv[2];
console.log(argument);
```

Si vous exécutez ce programme en tapant `node monProgramme.js Bonjour` dans votre terminal, la sortie sera `Bonjour`.

Vous pouvez également itérer à travers tous les arguments en utilisant une boucle `for`:

```Javascript
for (let i = 0; i < process.argv.length; i++) {
  console.log(process.argv[i]);
}
```

## Plongée profonde

En plus de récupérer les arguments passés à votre programme, vous pouvez également utiliser des options de ligne de commande en utilisant la bibliothèque `commander.js`. Avec cette bibliothèque, vous pouvez facilement définir des options et des commandes pour améliorer l'expérience utilisateur de vos programmes en ligne de commande.

Par exemple, vous pouvez définir une option `-v` qui affichera la version actuelle de votre programme. Voici un exemple de code utilisant `commander.js`:

```Javascript
const program = require('commander');

// Définit l'option -v pour afficher la version
program
  .option('-v, --version', 'Affiche la version de mon programme');

// Analyse les arguments de ligne de commande
program.parse(process.argv);

// Si -v est passé en argument, affiche la version
if (program.version) {
  console.log('La version actuelle de mon programme est 1.0.0');
}
```

En utilisant `commander.js`, vous pouvez créer des programmes en ligne de commande plus robustes et plus faciles à utiliser.

## Voir aussi

- [Documentation de Node.js sur process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Documentation de commander.js](https://github.com/tj/commander.js/#readme)
- [Article sur les arguments de ligne de commande en Javascript (en anglais)](https://www.twilio.com/blog/2017/12/http-requests-in-node-js.html)