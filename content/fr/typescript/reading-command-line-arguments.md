---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:56:53.412695-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire des arguments de ligne de commande, c'est récupérer des données directement de celui qui lance votre script. Les programmeurs font ça pour personnaliser l'exécution du programme, récupérer des configurations ou des informations essentielles sans interface utilisateur.

## How to:
TypeScript utilise le processus Node.js pour lire les arguments de ligne de commande. Voici un petit tuto.

```typescript
// Les arguments de ligne de commande sont stockés dans process.argv
const args = process.argv.slice(2); // On coupe les deux premiers éléments

// Affiche chaque argument passé
args.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// Exemple d’exécution: ts-node script.ts arg1 arg2
// Sortie attendue:
// 0: arg1
// 1: arg2
```

Utiliser `.slice(2)` est la clé ici. Il enlève les deux premières entrées du tableau `process.argv` qui sont le chemin de Node et le chemin du script.

## Deep Dive
Historiquement, les scripts en ligne de commande sont la base de l'interaction avec les systèmes d'exploitation. Aujourd'hui, même avec des interfaces graphiques sophistiquées, la ligne de commande reste précieuse pour sa rapidité et simplicité.

En TypeScript, on lit les arguments de la commande en utilisant `process.argv`, un tableau de chaînes où chaque entrée est un argument. Attention, tous les arguments sont lus comme des chaînes, donc si vous avez besoin d'autres types, vous devez les convertir.

Alternativement, des bibliothèques comme `yargs` ou `commander` améliorent l'expérience en fournissant parsing, validation, et messages d'aide, utile pour des outils plus complexes.

Implémentation détail: TypeScript, en tant que surcouche de JavaScript, ne fournit pas de mécanisme propre pour la lecture des arguments. Il s'appuie sur le runtime de Node.js, d'où l'usage de `process.argv`.

## See Also
Pour aller plus loin, voici quelques liens utiles:

- Documentation Node.js sur les arguments de la ligne de commande: [Node.js Process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- `yargs`, une bibliothèque pour le parsing des arguments: [yargs on NPM](https://www.npmjs.com/package/yargs)
- `commander`, une autre bibliothèque populaire: [commander on NPM](https://www.npmjs.com/package/commander)