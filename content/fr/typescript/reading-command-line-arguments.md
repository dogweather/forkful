---
title:    "TypeScript: Analyser les arguments de ligne de commande"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un outil utile pour passer des informations à un programme lors de son exécution. Cela permet aux utilisateurs de personnaliser leur expérience en fournissant des valeurs différentes sans avoir à modifier le code source du programme. La lecture des arguments de ligne de commande peut également être utile pour automatiser des tâches ou pour créer des scripts.

## Comment

Pour lire des arguments de ligne de commande en TypeScript, nous pouvons utiliser l'objet `process.argv` qui contient un tableau d'arguments. Le premier élément du tableau (`process.argv[0]`) est toujours le chemin vers l'exécutable node, le deuxième élément (`process.argv[1]`) est le chemin vers le fichier TypeScript en cours d'exécution, et les arguments réels commencent à partir de l'index 2.

Voici un exemple de code qui lit les arguments de ligne de commande et affiche le résultat :

```TypeScript
// arguments.ts

// Exemple de commande : node arguments.ts arg1 arg2

// Lire les arguments
const args: string[] = process.argv.slice(2);

// Afficher les arguments
console.log(args);
```

Output :

```bash
> node arguments.ts arg1 arg2
[ 'arg1', 'arg2' ]
```

Nous pouvons également utiliser la bibliothèque Commander pour gérer les arguments de ligne de commande de manière plus sophistiquée. Voici un exemple de code utilisant Commander pour lire les arguments de ligne de commande et afficher une aide :

```TypeScript
// commander.ts
import * as commander from 'commander';

// Exemple de commande : node commander.ts -n Nom -p 123

// Définition des options et des descriptions
commander
  .option('-n, --name <name>', 'Nom de l\'utilisateur')
  .option('-p, --password <password>', 'Mot de passe');

// Lecture des options
commander.parse(process.argv);

// Affichage d'une aide si aucun argument n'est fourni
if (!process.argv.slice(2).length) {
  commander.outputHelp();
}
```

Output :

```bash
> node commander.ts --help
Usage: commander [options]

Options:
  -n, --name <name>         Nom de l'utilisateur
  -p, --password <password> Mot de passe
  -h, --help                output usage information
```

## Plongée en profondeur

L'objet `process.argv` est une propriété de l'objet `process` qui est disponible dans l'environnement de Node.js. Cet objet peut être utilisé pour accéder à des informations sur le processus en cours, telles que les variables d'environnement et la gestion des flux d'entrée/sortie. L'utilisation de `process.argv` pour lire les arguments de ligne de commande est spécifique à Node.js et n'est pas prise en charge dans les navigateurs.

De plus, en utilisant la bibliothèque Commander, nous pouvons créer des options avec des valeurs par défaut, des options booléennes, des options requises et bien plus encore. Cette bibliothèque rend la lecture et la gestion des arguments de ligne de commande plus flexible et plus facile à utiliser.

## Voir aussi

- [Documentation officielle sur l'objet process](https://nodejs.org/api/process.html)
- [Documentation de Commander](https://github.com/tj/commander.js)
- [Les bases de TypeScript pour les débutants](http://www.journaldunet.com/web-tech/dictionnaire-du-webmastering/1447440-typescript-definition-syntaxe-et-bases-pour-les-debutants/)