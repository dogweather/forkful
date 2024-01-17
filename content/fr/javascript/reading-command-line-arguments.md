---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Javascript: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lire les arguments de ligne de commande est une méthode utilisée par les programmeurs pour obtenir des données en entrée lors de l'exécution d'un script ou d'un programme. Cela permet aux utilisateurs de fournir des informations spécifiques en utilisant la ligne de commande au lieu d'interagir avec une interface graphique.

## Comment faire:

Voici un exemple de code en Javascript pour lire les arguments de ligne de commande:

```Javascript
// Déclarer une variable pour stocker les arguments
let args = process.argv;

// Afficher les arguments passés en entrée
console.log("Les arguments de ligne de commande sont:", args);

// Accéder à un argument spécifique en utilisant son index
console.log("Le premier argument est:", args[2]);
```

Voici une sortie possible si les utilisateurs fournissent trois arguments en exécutant le script:

```
$ node monscript.js argument1 argument2 argument3
Les arguments de ligne de commande sont: [ 'node', 'monscript.js', 'argument1', 'argument2', 'argument3' ]
Le premier argument est: argument1
```

## Exploration approfondie:

Les arguments de ligne de commande ont été introduits pour la première fois dans les systèmes Unix en 1969, et ils sont encore largement utilisés aujourd'hui. Une alternative courante consiste à utiliser des options en tant qu'arguments au lieu de paramètres positionnels, mais cela nécessite une syntaxe plus complexe.

Lire les arguments de ligne de commande en Javascript est possible grâce à l'objet `process` qui contient un tableau `argv` avec tous les arguments passés en entrée. Il est également possible d'utiliser des modules externes, tels que `commander` ou `yargs`, pour faciliter la gestion des arguments de ligne de commande.

## Voir aussi:

- [Documentation officielle de node.js sur l'objet process](https://nodejs.org/dist/latest-v13.x/docs/api/process.html)
- [Commander - un module de ligne de commande pour node.js](https://github.com/tj/commander.js)
- [Yargs - une alternative à commander pour gérer les arguments de ligne de commande](https://github.com/yargs/yargs)