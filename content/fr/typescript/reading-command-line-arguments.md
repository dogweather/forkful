---
title:                "TypeScript: La lecture des arguments de ligne de commande"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi
Il peut être utile de comprendre comment lire les arguments de ligne de commande lors de la programmation en TypeScript. Cela permettra de traiter avec succès les entrées utilisateur et de créer des applications plus efficaces.

# Comment Faire
Lire les arguments de ligne de commande en TypeScript est très simple. Tout d'abord, importez le module "process" dans votre fichier TypeScript en ajoutant cette ligne en haut de votre fichier : 

```TypeScript
import process = require('process');
```

Ensuite, vous pouvez accéder à la liste des arguments de ligne de commande en utilisant la propriété "argv" du module process. Voici un exemple de code pour imprimer tous les arguments de ligne de commande et leur index :

```TypeScript
for (let i = 0; i < process.argv.length; i++) {
    console.log(`Argument ${i}: ${process.argv[i]}`);
}
```

Si vous exécutez ce code avec la commande "node file.ts arg1 arg2", la sortie sera la suivante :

```
Argument 0: node
Argument 1: file.ts
Argument 2: arg1
Argument 3: arg2
```

Vous pouvez également utiliser la méthode "slice" pour ignorer les deux premiers arguments ("node" et le nom du fichier) et n'afficher que les arguments fournis par l'utilisateur :

```TypeScript
for (let i = 2; i < process.argv.length; i++) {
    console.log(`Argument ${i - 2}: ${process.argv[i]}`);
}
```

# Plongée Profonde
Le module "process" offre également d'autres propriétés et méthodes utiles pour travailler avec les arguments de ligne de commande. Par exemple, vous pouvez utiliser "process.cwd()" pour obtenir le dossier de travail en cours, ou "process.env" pour accéder aux variables d'environnement du système.

Pour les applications complexes, vous pouvez également utiliser des packages externes comme "commander" ou "yargs" pour gérer les arguments de ligne de commande de manière plus structurée et flexible.

# Voir Aussi
- Documentation officielle de process.argv en Node.js : https://nodejs.org/api/process.html#process_process_argv
- Exemple de code pour lire les arguments de ligne de commande en TypeScript : https://github.com/torokmark/node-typescript-cli-boilerplate/blob/master/src/utils/cli.ts