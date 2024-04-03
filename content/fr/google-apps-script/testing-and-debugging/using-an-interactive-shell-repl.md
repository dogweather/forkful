---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:38.754325-07:00
description: "Comment faire : Google Apps Script, un langage de script bas\xE9 sur\
  \ le cloud pour automatiser les t\xE2ches \xE0 travers les produits Google, ne dispose\
  \ pas d'un\u2026"
lastmod: '2024-03-13T22:44:57.187861-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, un langage de script bas\xE9 sur le cloud pour automatiser\
  \ les t\xE2ches \xE0 travers les produits Google, ne dispose pas d'un outil REPL\
  \ int\xE9gr\xE9 similaire \xE0 ceux des langages comme Python ou le Node.js de JavaScript."
title: Utiliser un shell interactif (REPL)
weight: 34
---

## Comment faire :
Google Apps Script, un langage de script basé sur le cloud pour automatiser les tâches à travers les produits Google, ne dispose pas d'un outil REPL intégré similaire à ceux des langages comme Python ou le Node.js de JavaScript. Cependant, vous pouvez simuler une expérience similaire en utilisant les fonctionnalités de journalisation et de débogage de l'éditeur Apps Script ou en configurant un environnement externe. Ici, nous nous concentrons sur la création d'un REPL de fortune à l'intérieur de l'éditeur Apps Script.

1. **Création d'une fonction REPL de fortune** :

```javascript
function myREPL() {
  var input = Logger.log('Entrez votre expression : ');
  try {
    var result = eval(input);
    Logger.log('Résultat : ' + result);
  } catch(e) {
    Logger.log('Erreur : ' + e.message);
  }
}
```

Comme l'entrée directe de l'utilisateur n'est pas faisable de la même manière qu'un REPL traditionnel dans l'environnement Apps Script, vous pouvez modifier manuellement la variable `input` et exécuter `myREPL()` pour tester des expressions.

2. **Exécution de Code Exemple** :

Disons que vous souhaitez évaluer `2+2`. Vous modifieriez la fonction `myREPL` comme suit :

```javascript
function myREPL() {
  var input = '2+2'; // Entrez manuellement votre expression ici
  // Le reste reste inchangé...
}
```

Après avoir exécuté `myREPL()`, vérifiez les Journaux (Vue > Journaux) pour la sortie, qui devrait lire quelque chose comme :

```
[20-xx-xxxx xx:xx:xx:xxx] Entrez votre expression :
[20-xx-xxxx xx:xx:xx:xxx] Résultat : 4
```

3. **Débogage avec Logger** :

Pour un débogage plus complexe, intercalez `Logger.log(variable);` dans votre code pour imprimer les états des variables, vous aidant à comprendre le flux et les états intermédiaires de vos scripts.

## Approfondissement
Le concept de REPL est profondément ancré dans l'histoire de l'informatique, découlant des systèmes de partage de temps des années 1960 qui permettaient des sessions interactives. Des langages comme Lisp ont prospéré dans cet environnement, car le REPL était crucial pour leur processus de développement itératif. En contraste, Google Apps Script, émergent bien plus tard, est principalement conçu pour le web, se concentrant sur l'automatisation des tâches au sein de la suite Google plutôt que sur la programmation basée sur console et itérative.

Google Apps Script ne supporte pas traditionnellement les sessions de codage interactives en temps réel dès le départ en raison de sa nature basée sur le cloud et de son orientation vers le déploiement d'applications web. Son modèle d'exécution tourne autour des fonctions déclenchées par des événements web, des déclencheurs temporisés, ou l'invocation manuelle à l'intérieur de l'environnement, plutôt que des boucles de feedback instantanées fournies par un REPL.

Bien que le REPL de fortune et le débogueur à l'intérieur de l'éditeur Apps Script offrent un certain niveau d'interactivité, ils ne reproduisent pas pleinement le feedback immédiat et l'efficacité des REPLs traditionnels que l'on trouve dans de nombreux langages de programmation. Les développeurs à la recherche d'une expérience REPL plus authentique avec les technologies Google pourraient explorer les environnements JavaScript externes ou Node.js avec les API de Google. Ils peuvent fournir une session de codage plus réactive et interactive, bien que nécessitant plus de configuration et éventuellement de sortir de l'environnement Apps Script direct.
