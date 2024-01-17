---
title:                "La lecture des arguments de ligne de commande"
html_title:           "TypeScript: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le lire?
Lire les arguments de la ligne de commande est une pratique courante chez les programmeurs. Cela consiste à récupérer les données saisies par l'utilisateur lors de l'exécution d'un programme en ligne de commande. Les arguments peuvent être utilisés pour personnaliser l'exécution du programme ou pour fournir des informations supplémentaires à l'utilisateur.

## Comment le faire:
Voici un exemple de code en TypeScript pour lire les arguments de la ligne de commande et les afficher dans la console:

```TypeScript
const args = process.argv.slice(2); // Récupère les arguments de la ligne de commande en excluant les deux premiers (node et le chemin du script)
console.log(args); // Affiche les arguments dans la console
```

Si l'utilisateur exécute le programme avec la commande `node index.ts hello world`, cela affichera `['hello', 'world']` dans la console.

## Exploration en profondeur:
Cette pratique de lecture des arguments de la ligne de commande est courante dans les langages de programmation et remonte aux premiers systèmes d'exploitation où le seul moyen de communiquer avec l'ordinateur était via la ligne de commande. Une alternative à cette méthode est d'utiliser une interface utilisateur graphique (GUI), mais cela peut être moins pratique pour certains types de programmes.

Pour lire les arguments de la ligne de commande en TypeScript, nous utilisons l'objet `process` qui est fourni par l'environnement d'exécution Node.js. L'objet `argv` de cet objet est un tableau contenant les arguments, où le premier élément est le chemin de l'exécutable node et le deuxième est le chemin du script en cours d'exécution. En utilisant la méthode `slice` pour exclure ces deux premiers éléments, nous obtenons un tableau contenant uniquement nos arguments.

## Voir aussi:
Si vous souhaitez en savoir plus sur la manière dont les arguments de la ligne de commande sont traités en TypeScript, vous pouvez consulter la documentation officielle de Node.js: [process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv). Vous pouvez également lire cet article pour découvrir d'autres astuces utiles en TypeScript: [10 astuces pour améliorer votre code TypeScript](https://dzone.com/articles/10-useful-typescript-tips-to-improve-your-code).