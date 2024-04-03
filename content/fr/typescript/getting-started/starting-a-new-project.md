---
date: 2024-01-20 18:04:45.740781-07:00
description: "Quand on d\xE9marre un nouveau projet en TypeScript, on met en place\
  \ les bases du code. Les programmeurs font \xE7a pour structurer et organiser leurs\
  \ id\xE9es\u2026"
lastmod: '2024-03-13T22:44:57.437701-06:00'
model: gpt-4-1106-preview
summary: "Quand on d\xE9marre un nouveau projet en TypeScript, on met en place les\
  \ bases du code."
title: Lancement d'un nouveau projet
weight: 1
---

## How to:
Installer TypeScript et démarrer un nouveau projet est simple et direct.

```shell
# Installer TypeScript globalement
npm install -g typescript

# Initialiser un nouveau projet
mkdir monProjetTS
cd monProjetTS
npm init -y
tsc --init

# Créer un fichier simple pour le tester
echo "console.log('Salut TypeScript !');" > index.ts

# Compiler le fichier TypeScript
tsc

# Exécuter le JavaScript compilé
node index.js
```

Le contenu de `index.js` après compilation sera :

```TypeScript
console.log('Salut TypeScript !');
```

L'exécution via Node.js affichera :

```
Salut TypeScript !
```

## Deep Dive
TypeScript est un sur-ensemble de JavaScript créé par Microsoft en 2012. Cela signifie que tout code JavaScript valide est aussi un code TypeScript valide, mais avec des fonctionnalités supplémentaires comme le typage statique.

Pourquoi TypeScript et pas simplement JavaScript ? TypeScript ajoute la sécurité des types et facilite la gestion de gros projets. Sans TypeScript, les gros projets JavaScript peuvent vite devenir chaotiques.

Il existe des alternatives comme Flow, mais TypeScript est plus populaire, surtout dans des projets qui ont besoin d'une grande échelle.

En pratique, `tsc --init` crée un fichier `tsconfig.json`, qui est le cœur de tout projet TypeScript. Ce fichier contient la configuration du compilateur; vous pouvez y régler des paramètres comme la version cible de JavaScript et les modules.

## See Also
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
- [TypeScript Deep Dive](https://basarat.gitbook.io/typescript/)
- [Compiler Options in TypeScript](https://www.typescriptlang.org/tsconfig)
