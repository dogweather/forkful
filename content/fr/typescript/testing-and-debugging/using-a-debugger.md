---
date: 2024-01-26 04:11:08.533785-07:00
description: "Pour commencer avec un d\xE9bogueur en TypeScript, tout ce dont vous\
  \ avez besoin est un IDE pris en charge (comme Visual Studio Code) et une configuration\u2026"
lastmod: '2024-03-13T22:44:57.441720-06:00'
model: gpt-4-0125-preview
summary: "Pour commencer avec un d\xE9bogueur en TypeScript, tout ce dont vous avez\
  \ besoin est un IDE pris en charge (comme Visual Studio Code) et une configuration\u2026"
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Pour commencer avec un débogueur en TypeScript, tout ce dont vous avez besoin est un IDE pris en charge (comme Visual Studio Code) et une configuration `launch.json`. Voici un exemple rapide pour une application Node.js :

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Bonjour, ${name} !`);
}

const userName = 'Ada';
greet(userName);
```

Pour déboguer ceci, créez un fichier `launch.json` sous le dossier `.vscode` :

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Lancer le programme",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Ensuite, placez un point d'arrêt dans votre fonction `greet` en cliquant sur le côté gauche du numéro de ligne dans votre IDE. Appuyez sur F5 pour commencer le débogage, et regardez votre application se mettre en pause au point d'arrêt. Vous pouvez maintenant survoler les variables, observer les expressions et parcourir votre code avec aisance.

## Plongée Profonde
Dans le passé, avant que les environnements de développement intégrés (IDE) ne deviennent élégants, le débogage se faisait souvent avec des instructions d'impression (connues sous le nom de débogage `console.log`). Cela fonctionnait, d'une certaine manière, mais c'était comme chercher une aiguille dans une botte de foin les yeux bandés.

Les débogueurs modernes sont comme un couteau suisse pour le dépannage. Avec l'évolution de TypeScript et Node.js, divers débogueurs sont disponibles, allant de l'inspecteur Node.js intégré aux outils de développement de navigateur pour le débogage côté client.

L'inspecteur Node.js fonctionne en se connectant à votre application en cours d'exécution ; il communique via le protocole Chrome DevTools, transformant votre navigateur Chrome en une puissante console de débogage. Cette intégration permet une session de débogage visuellement interactive et détaillée, par rapport aux pratiques de débogage en ligne de commande traditionnelles.

## Voir Aussi
Pour un peu de lecture supplémentaire et quelques conseils de pro, consultez :

- [Débogage TypeScript dans Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Guide de débogage Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Documentation des outils de développement Chrome](https://developers.google.com/web/tools/chrome-devtools)
