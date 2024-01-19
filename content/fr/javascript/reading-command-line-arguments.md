---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lire les arguments de ligne de commande en Javascript : Un guide de survie rapide

Le langage de programmation Javascript ne cesse de se développer. Dans cet article, nous aborderons un aspect clé : la lecture des arguments de ligne de commande en Javascript.

## Qu'est-ce que c'est & Pourquoi ?
La lecture d'arguments de ligne de commande en Javascript désigne l'extraction des paramètres passés à un script lors de son exécution sur un terminal. Les développeurs l'utilisent pour personnaliser le comportement d'un script selon les valeurs fournies.

## Comment faire :

### Exemple:

```Javascript
// index.js
let myArgs = process.argv.slice(2); 
console.log('myArgs: ', myArgs);
```

### Exécution:

```command line
node index.js chocolate vanilla
```

### Sortie:

```command line
myArgs:  [ 'chocolate', 'vanilla' ]
```

Dans l'exemple ci-dessus, `index.js` est notre script et `chocolate` et `vanilla` sont les arguments que nous avons passés. Lorsque nous exécutons le script avec Node.js, il enregistre tous les arguments de la ligne de commande dans le tableau `process.argv`.

## Plongée en profondeur :

Historiquement, Javascript était principalement utilisé pour les scripts côté client dans les navigateurs. Cependant, avec l'arrivée de Node.js en 2009, nous avons pu exécuter Javascript côté serveur, y compris le traitement des arguments de ligne de commande.

Il existe d'autres bibliothèques, comme `commander` et `yargs`, qui offrent plus de fonctionnalités pour la gestion des arguments. Celles-ci peuvent inclure le parsing d'arguments, la génération d'aide, et la validation d'arguments.

En interne, Node.js utilise le module `process` pour interagir avec le système d'exploitation sous-jacent. Le tableau `process.argv` contient les arguments de la ligne de commande, le premier élément étant 'node', le deuxième étant le nom du script, et les éléments suivants étant les arguments passés au script.

## Voir aussi :

1. Documentation Node.js sur le processus : https://nodejs.org/docs/latest/api/process.html#process_process_argv
2. Bibliothèque Commander.js : https://github.com/tj/commander.js/
3. Bibliothèque Yargs : https://github.com/yargs/yargs