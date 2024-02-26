---
date: 2024-01-20 17:56:18.424998-07:00
description: "Lire les arguments de ligne de commande, c'est r\xE9cup\xE9rer les donn\xE9\
  es que les utilisateurs passent \xE0 votre programme Node.js quand ils le lancent.\
  \ On fait\u2026"
lastmod: '2024-02-25T18:49:54.923132-07:00'
model: gpt-4-1106-preview
summary: "Lire les arguments de ligne de commande, c'est r\xE9cup\xE9rer les donn\xE9\
  es que les utilisateurs passent \xE0 votre programme Node.js quand ils le lancent.\
  \ On fait\u2026"
title: Lecture des arguments de ligne de commande
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Lire les arguments de ligne de commande, c'est récupérer les données que les utilisateurs passent à votre programme Node.js quand ils le lancent. On fait ça pour personnaliser l'exécution ou pour donner des instructions spécifiques au programme.

## Comment faire :
Voici un petit exemple. Vous écrivez un script, l'utilisateur le lance avec des arguments, et votre script les utilise.

```Javascript
// process.argv a tout ce dont vous avez besoin
const arguments = process.argv.slice(2); // On coupe les deux premiers éléments

// Affiche les arguments passés
console.log('Arguments de ligne de commande:', arguments);

// Utilisation simple : node script.js argument1 argument2
```

Lancez votre script ainsi :

```bash
node script.js pomme banane
```

Sortie attendue :

```
Arguments de ligne de commande: [ 'pomme', 'banane' ]
```

## Plongée Profonde :
Historiquement, les arguments de ligne de commande sont un moyen de communiquer avec les programmes en mode texte depuis les premiers jours de l'informatique. 
En JavaScript, le processus Node.js expose l'objet `process` global qui donne accès aux arguments via `process.argv`. C'est un tableau et les deux premiers éléments sont le chemin de Node.js et le fichier script. Donc, `slice(2)` est nécessaire pour obtenir seulement les arguments qui vous intéressent.

Il existe aussi des librairies comme `yargs` ou `commander` pour rendre la gestion des arguments plus robuste et les parsent automatiquement pour vous. Ces librairies supportent des choses comme les options par défaut, les alias, et la validation.

## Voir Aussi :
- Documentation Node.js sur `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- NPM `yargs`: https://www.npmjs.com/package/yargs
- NPM `commander`: https://www.npmjs.com/package/commander
