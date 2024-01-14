---
title:                "TypeScript: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est courant de rencontrer des problèmes lors de la programmation en langage TypeScript (ou tout autre langage de programmation d'ailleurs). La débogage manuel est fastidieux et prend beaucoup de temps. C'est là que l'impression de sortie de débogage entre en jeu. Elle vous permet de voir les valeurs des variables et les étapes de votre code en temps réel, vous aidant à identifier rapidement les problèmes dans votre code. 

## Comment faire

Dans TypeScript, il existe deux façons d'imprimer une sortie de débogage : en utilisant la fonction `console.log` et en utilisant le débogueur intégré de TypeScript. Voici deux exemples de code qui illustrent ces deux méthodes :

```TypeScript
// Utilisation de console.log
let nombre = 10;
console.log("Le nombre est", nombre);

// Utilisation du débogueur intégré
let somme = 0;
for(let i=1; i<=10; i++){
    somme += i;
}
// Démarrez le débogueur en appuyant sur F5 dans VS Code
// Placez un point d'arrêt à la ligne suivante
console.log("La somme est", somme);
```

La première méthode utilise la fonction `console.log` pour imprimer la valeur de la variable `nombre`. La deuxième méthode utilise le débogueur intégré de TypeScript en utilisant un point d'arrêt pour afficher la valeur de la variable `somme` à un moment précis dans le code. 

## Plongée profonde

L'impression de sortie de débogage peut être utile non seulement pour afficher les valeurs des variables, mais aussi pour suivre les étapes d'exécution de votre code. En utilisant des points d'arrêt, vous pouvez exécuter votre code ligne par ligne et voir comment les valeurs des variables changent à chaque étape. De plus, le débogueur intégré de TypeScript offre des fonctionnalités telles que la possibilité d'ajouter des montres pour surveiller les valeurs des variables spécifiques et de gérer les erreurs de manière plus efficace.

## Voir aussi

- [Guide de débogage TypeScript](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Déboguer votre code TypeScript avec VS Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Utilisation de console.log dans TypeScript](https://www.tutorialsteacher.com/typescript/typescript-console)