---
title:                "Ecriture vers l'erreur standard"
html_title:           "TypeScript: Ecriture vers l'erreur standard"
simple_title:         "Ecriture vers l'erreur standard"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation de la fonction de sortie standard d'erreur (standard error) peut sembler intimidante pour les développeurs débutants, mais c'est en fait un outil très utile pour déboguer du code en fournissant des informations détaillées sur les erreurs rencontrées lors de l'exécution de votre programme.

## Comment faire

C'est très simple d'écrire vers la sortie standard d'erreur en TypeScript. Tout d'abord, vous devez importer la classe "process" en haut de votre fichier :

```TypeScript
import { process } from 'process';
```

Ensuite, vous pouvez utiliser la méthode "stderr.write" pour écrire un message vers la sortie standard d'erreur :

```TypeScript
process.stderr.write("Une erreur est survenue !");
```

Vous pouvez également utiliser des placeholders pour inclure des variables dans votre message :

```TypeScript
let nom = "John";
let age = 25;

process.stderr.write(`Bonjour ${nom}, tu as ${age} ans.`);
```

Lorsque votre programme s'exécutera, le message sera écrit dans la console avec un préfixe "Erreur" pour vous aider à le repérer facilement :

```
Erreur: Bonjour John, tu as 25 ans.
```

## Plongeon en profondeur

Maintenant que vous savez comment utiliser la sortie standard d'erreur en TypeScript, il est important de comprendre quand l'utiliser. Il est recommandé d'écrire vers la sortie standard d'erreur pour les erreurs critiques qui peuvent causer des dysfonctionnements ou des plantages du programme. Vous pouvez également utiliser cette fonctionnalité pour fournir des informations de débogage détaillées lors du développement de votre application.

Il est également important de noter que la sortie standard d'erreur est différente de la sortie standard (standard output). La sortie standard est utilisée pour afficher des messages normaux, tandis que la sortie standard d'erreur est spécifiquement conçue pour afficher des erreurs. Vous pouvez donc utiliser les deux méthodes en combinaison pour améliorer la façon dont vous gérez les messages dans votre code.

## Voir aussi

- [Documentation de la classe Process en TypeScript](https://nodejs.org/api/process.html)
- [Guide pour le débogage en TypeScript](https://basarat.gitbook.io/typescript/debugging)
- [Utiliser la fonction console.error() en JavaScript](https://developer.mozilla.org/fr/docs/Web/API/Console/error)