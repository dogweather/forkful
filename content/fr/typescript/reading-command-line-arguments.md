---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lecture des arguments de la ligne de commande en TypeScript

## Qu'est-ce que & Pourquoi ?

Traiter les arguments de la ligne de commande signifie lire les informations directement passées au script lors de son exécution. Les programmeurs le font pour personnaliser le comportement d'un programme ou pour spécifier les données à traiter.

## Comment faire :

Le module `process` de Node.js rend les arguments de la ligne de commande accessibles via `process.argv`, un tableau contenant les valeurs passées. Illustrons cela avec un exemple de code.

```TypeScript
// Fichier: args.ts
for(let i = 0; i < process.argv.length; i++) {
  console.log(`Argument ${i}: ${process.argv[i]}`);
}
```

Exécutons ce script avec des arguments:

```
$ node args.ts param1 "second param"
Argument 0: /path/to/node
Argument 1: /path/to/args.ts
Argument 2: param1
Argument 3: second param
```

## Plongée en profondeur 

Historiquement, l'accès aux arguments de la ligne de commande dans les langages de script a toujours été une fonction importante. C'est une façon intuitive et flexible d'interagir avec les scripts.

En TypeScript (et JavaScript), nous passons par le module Node.js `process`. Bien sûr, diverses bibliothèques peuvent simplifier et augmenter ce processus, comme `yargs` et `commander`.

Pour les détails d'implémentation, `process.argv[0]` contient toujours le chemin de l'interpréteur Node.js, `process.argv[1]` le chemin du script en cours d'exécution. Les arguments réels commencent donc à `process.argv[2]`.

## Voir aussi 

- Documentation Node.js pour [Process](https://nodejs.org/api/process.html)
- Bibliothèque [Commander](https://www.npmjs.com/package/commander)
- Bibliothèque [Yargs](https://www.npmjs.com/package/yargs) 

En travaillant avec les arguments de la ligne de commande, les possibilités sont nombreuses. La limite est vraiment votre imagination, ou plutôt celle de vos scripts.