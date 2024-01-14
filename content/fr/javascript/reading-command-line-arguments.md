---
title:    "Javascript: Lecture des arguments de la ligne de commande"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Javascript intéressé par l'apprentissage de techniques avancées, alors la lecture des arguments de ligne de commande peut être une compétence très utile à acquérir. Cela peut vous permettre de créer des programmes plus flexibles et plus efficaces en utilisant les informations fournies par l'utilisateur à travers la ligne de commande.

## Comment faire

Pour lire les arguments de ligne de commande en Javascript, vous pouvez utiliser l'objet `process` intégré à Node.js. Voici un exemple de code qui montre comment accéder à ces arguments:

```Javascript
// Récupérer les arguments de la ligne de commande
const arguments = process.argv;

// Parcourir chaque argument et l'afficher
arguments.forEach((argument, index) => console.log(`Argument ${index}: ${argument}`));
```

Supposons que vous ayez enregistré ce code dans un fichier appelé `arguments.js`. Vous pouvez alors l'exécuter en utilisant la commande suivante dans votre terminal:

```
node arguments.js arg1 arg2 arg3
```

Cela donnera la sortie suivante:

```
Argument 0: /usr/local/bin/node
Argument 1: /Users/votre-nom/fichier/arguments.js
Argument 2: arg1
Argument 3: arg2
Argument 4: arg3
```

Comme vous pouvez le constater, le premier argument est le chemin vers l'exécutable Node.js et le deuxième est le chemin vers votre fichier. Les arguments définis par vous sont affichés après cela.

## Plongée en profondeur

Vous pouvez également accéder aux arguments directement en utilisant l'index, par exemple `process.argv[2]` pour récupérer le premier argument. De plus, il existe des packages tels que `yargs` qui peuvent vous aider à gérer les arguments de manière plus structurée et à créer des interfaces utilisateur en ligne de commande plus avancées.

Il est également important de noter que les arguments de ligne de commande peuvent être très utiles pour automatiser des tâches répétitives ou pour exécuter plusieurs versions de votre code en utilisant des combinaisons d'arguments différentes.

## Voir aussi

- [Documentation officielle de Node.js sur les arguments de ligne de commande](https://nodejs.org/api/process.html#process_process_argv)
- [Documentation de `yargs` pour la gestion facile des arguments en ligne de commande](https://github.com/yargs/yargs)