---
title:                "TypeScript: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous déjà utilisé des applications en ligne de commande et nous avons peut-être remarqué que certaines peuvent prendre des entrées en ligne de commande, couramment appelées arguments. Les arguments en ligne de commande permettent à l'utilisateur de fournir des informations ou des instructions à une application lors de son exécution. Dans ce billet, nous allons discuter de la lecture des arguments en ligne de commande en utilisant TypeScript et de leur importance dans la création d'applications.

## Comment faire

Pour lire les arguments en ligne de commande en TypeScript, nous pouvons utiliser l'objet `process` qui est disponible dans Node.js. Nous pouvons accéder à cet objet en important `process` dans notre fichier TypeScript. Voici un exemple de code montrant comment lire le premier argument en ligne de commande passé à notre application :

```TypeScript
import process from 'process';

const firstArgument = process.argv[2];
console.log(`Argument en ligne de commande : ${firstArgument}`);
```

Si nous exécutons cette application avec la commande `node index.js hello`, nous obtiendrons la sortie suivante :

```
Argument en ligne de commande : hello
```

Nous pouvons également lire plusieurs arguments en utilisant une boucle `for` :

```TypeScript
import process from 'process';

for (let i = 2; i < process.argv.length; i++) {
  console.log(`Argument ${i - 1} : ${process.argv[i]}`);
}
```

Si nous exécutons cette application avec la commande `node index.js foo bar baz`, nous obtiendrons la sortie suivante :

```
Argument 1 : foo
Argument 2 : bar
Argument 3 : baz
```

## Profonde plongée

Maintenant que nous savons comment lire les arguments en ligne de commande en utilisant TypeScript, il est important de comprendre comment ils peuvent être utiles dans nos applications. Les arguments en ligne de commande peuvent être utilisés pour fournir des options ou des paramètres à notre application, tels que spécifier un fichier d'entrée ou de sortie, un mode de fonctionnement ou encore des informations d'identification.

Nous pouvons également utiliser des bibliothèques telles que `yargs` pour traiter et valider les arguments en ligne de commande de manière plus structurée et intuitive.

## Voir aussi

- [Documentation de `process` dans Node.js](https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Documentation de `yargs`](https://github.com/yargs/yargs)