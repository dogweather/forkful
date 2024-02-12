---
title:                "Utilisation d'une console interactive (REPL)"
aliases:
- /fr/typescript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:26.356536-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation d'une console interactive (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Un Read-Eval-Print-Loop (REPL) est un environnement de programmation qui prend des entrées utilisateur uniques, les exécute et retourne le résultat à l'utilisateur. Les programmeurs utilisent un REPL pour expérimenter rapidement avec des extraits de code, déboguer et apprendre de nouvelles fonctionnalités de langage sans la nécessité de créer une application complète.

## Comment faire :
TypeScript ne vient pas avec son propre REPL. Utilisons `ts-node`, un environnement d'exécution TypeScript pour Node.js qui inclut un REPL.

D'abord, installez-le globalement :
```bash
npm install -g ts-node
```

Démarrez le REPL en tapant `ts-node` dans votre ligne de commande :
```bash
ts-node
```

Voici un rapide extrait à essayer :
```TypeScript
> let message: string = 'Bonjour, REPL !';
> console.log(message);
Bonjour, REPL !
> 
```
Pour terminer la session, appuyez sur `Ctrl+D`.

## Plongée en profondeur
Historiquement, les REPL étaient prééminents dans des langages comme Lisp, permettant une évaluation dynamique du code. Le concept s'est depuis répandu, devenant un élément de base pour le codage interactif dans de nombreux langages.

Pour TypeScript, `ts-node` n'est pas votre seule option. Les alternatives incluent l'utilisation du TypeScript Playground dans un navigateur web ou le recours à d'autres REPLs basés sur Node.js qui supportent TypeScript avec des plugins adaptés.

En termes d'implémentation, `ts-node` utilise l'API du compilateur TypeScript pour transpiler le code à la volée avant qu'il ne soit exécuté par Node.js. Cela vous donne un retour immédiat et est particulièrement utile pour essayer les dernières fonctionnalités de TypeScript sans les tracas de configuration.

Une chose à retenir - bien qu'un REPL soit excellent pour des tests rapides, cela ne remplace pas l'écriture de code traditionnel, testable et maintenable. C'est un outil d'apprentissage et d'exploration, pas un substitut aux pratiques de développement appropriées.

## Voir aussi
- [Site officiel de TypeScript](https://www.typescriptlang.org/)
- [ts-node sur GitHub](https://github.com/TypeStrong/ts-node)
- [Documentation REPL de Node.js](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
